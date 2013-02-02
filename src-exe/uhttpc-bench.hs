{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lex.Integral (readHexadecimal)
import qualified Data.ByteString.Unsafe as B
import           Data.Data
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           GHC.Conc.Sync (setNumCapabilities,getNumCapabilities,getNumProcessors)
import           Network.HTTP.MicroClient
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.URI
import           System.Console.CmdArgs.Implicit
import           System.IO
import           System.Mem (performGC)
import           Text.Printf

-- |Timestamp in seconds since POSIX epoch
type TS = Double

getTS :: IO TS
getTS = fmap realToFrac getPOSIXTime

-- |single measurement entry
data Entry = Entry
    { entConnId    :: {-# UNPACK #-} !Word -- each connections gets a new id
    , entRespCode  :: {-# UNPACK #-} !Int
    , entRespRLen  :: {-# UNPACK #-} !Word64 -- total amount of data received
    , entRespCLen  :: {-# UNPACK #-} !Word64 -- payload content-length
    , entConnect   :: {-# UNPACK #-} !TS   -- ts at connection-startup
    , entReqStart  :: {-# UNPACK #-} !TS   -- ts before sending request
    , entReqDone   :: {-# UNPACK #-} !TS   -- ts after sending request
    , entRespStart :: {-# UNPACK #-} !TS   -- ts when response starts being received
    , entRespDone  :: {-# UNPACK #-} !TS   -- ts when full response has been received
    } deriving (Show)

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

data Args = Args
    { argNumReq    :: Word
    , argThrCnt    :: Word
    , argClnCnt    :: Word
    , argKeepAlive :: Bool
    , argCsv       :: FilePath
    , argHdrs      :: [String]
    , argUrl       :: String
    } deriving (Show,Data,Typeable)

newtype MURI = MURI String

instance Default Args where
    def = Args
        { argNumReq    = 1 &= typ "num" &= explicit &= name "n"
                         &= help "number of requests    (default: 1)"
        , argThrCnt    = 1 &= typ "num" &= explicit &= name "t"
                         &= help "threadcount           (default: 1)"
        , argClnCnt    = 1 &= typ "num" &= explicit &= name "c"
                         &= help "concurrent clients    (default: 1)"
        , argKeepAlive = False &= explicit &= name "k"
                         &= help "keep alive            (default: no)"
        , argHdrs      = def &= typ "str" &= explicit &= name "H"
                         &= help "add header to request"
        , argCsv       = def &= typFile &= explicit &= name "csv"
                         &= help "dump CSV data         (default: none)"
        , argUrl       = def &= argPos 0 &= typ "<url>"
        } &= program "uttpc-bench"
          &= summary "Simple HTTP benchmark tool modelled after weighttp's CLI"

-- |Split HTTP URL into (hostname,port,url-path)
splitUrl :: String -> Either String (String,PortNumber,String)
splitUrl url0 = do
    uri <- note "invalid URI" $ parseAbsoluteURI url0

    unless (uriScheme uri == "http:") $
        Left "URI must have 'http' scheme"

    urlauth <- note "missing host-part in URI" $ uriAuthority uri
    let hostname = uriRegName urlauth

    when (null hostname) $
        Left "empty hostname in URL"

    unless (null . uriUserInfo $ urlauth) $
        Left "user/pass in URL not supported"

    portnum <- case uriPort urlauth of
        ""      -> return 80
        ':':tmp -> return $! fromIntegral (read tmp :: Word)
        _       -> Left "invalid port-number"

    return (hostname,portnum,if null (uriPath uri) then "/" else uriPath uri)

main :: IO ()
main = do
    pargs <- cmdArgs def
    print (pargs :: Args)

    (hostname,portnum,urlpath) <- either fail return $ splitUrl (argUrl pargs)

    -- sanity-check CLI arguments
    unless (argNumReq pargs > 0) $
        fail "request-count must be > 0"

    unless (argClnCnt pargs > 0) $
        fail "client-count must be > 0"

    unless (argThrCnt pargs > 0) $
        fail "thread-count must be > 0"

    unless (argNumReq pargs >= argClnCnt pargs) $
        fail "request-count must be >= client-count"

    -- setup socket address
    sa <- getSockAddr hostname portnum

    -- set up worker threads
    when (argThrCnt pargs > 1) $
        setNumCapabilities (fromIntegral $ argThrCnt pargs)
    thrcnt' <- fmap fromIntegral getNumCapabilities

    when (argThrCnt pargs > 1 && thrcnt' /= argThrCnt pargs) $
        fail "failed to execute request tread-count"

    numCpus <- fmap fromIntegral getNumProcessors

    when (thrcnt' > numCpus) $
        printf "*WARNING* thread-count (%u) > cpu cores (%u)\n" thrcnt' numCpus

    when (thrcnt' > 1) $
        printf "using up to %u Haskell execution threads\n" thrcnt'

    unless (argClnCnt pargs >= thrcnt') $
        putStrLn "*WARNING* client-count should be >= thread-count"

    ----------------------------------------------------------------------------
    let rawreq = B8.intercalate "\r\n" $
                 [ "GET " <> B8.pack urlpath <> " HTTP/1.1"
                 , "Host: " <> B8.pack hostname <> ":" <> B8.pack (show portnum)
                 , "User-Agent: uttpc-bench"
                 ] ++
                 map B8.pack (argHdrs pargs) ++
                 [ ""
                 , "" -- body
                 ]

    putStrLn $ "using request: " ++ show rawreq ++ "\n"
    putStrLn "starting benchmark..."

    reqCounter <- newIORef (fromIntegral (argNumReq pargs) :: Int)

    let act j = do
            ts0 <- getTS
            ts' <- doReqs sa j reqCounter rawreq
            ts1 <- getTS
            return (ts0,ts1,ts')

    performGC
    ts0' <- getTS
    tss  <- mapConcurrently act [1 .. argClnCnt pargs]
    ts1' <- getTS
    performGC

    numReqs' <- readIORef reqCounter
    unless (numReqs' + (fromIntegral $ argClnCnt pargs) == 0) $ do
        fail "virtual clients exited prematurely!"

    putStrLn "\nper-client stats:\n"

    forM_ tss $ \(ts0,ts1,ents') -> do
        let n' = length ts'
            ts' = map (\e -> entRespDone e - entReqStart e) ents'
            tdelta = ts1-ts0

        _ <- printf " client spawned +%.6f s, %d reqs, %.1f req/s, finished in %.6f s\n"
                    (ts0-ts0') n' (fromIntegral n' / tdelta) tdelta
        _ <- printf " rtt min/avg/max = %.3f/%.3f/%.3f ms\n" (1000*minimum ts') (1000*avg ts') (1000*maximum ts')
        putStrLn ""

    let allents = sortBy (comparing entConnect) $ concatMap (\(_,_,ents) -> ents) tss
        (rlen,clen) = foldl' (\(r,c) e -> (r + entRespRLen e,c + entRespCLen e)) (0,0) allents

    _ <- printf "finished in %.6f seconds, %.1f req/s received\n"
                (ts1'-ts0') (fromIntegral (argNumReq pargs) / (ts1'-ts0'))

    _ <- printf "data received: %.3f KiB/s, %u bytes total (%u bytes http + %u bytes content)\n"
                (fromIntegral rlen / (1024 * (ts1'-ts0'))) rlen (rlen-clen) clen

    ----------------------------------------------------------------------------
    -- CSV dumping
    unless (null $ argCsv pargs) $ do
        putStrLn "dumping CSV..."
        withFile (argCsv pargs) WriteMode $ \h -> do
            _ <- hPrintf h "timestamp_s,connect_ms,req_send_ms,resp_wait_ms,resp_recv_ms,resp_status,resp_total_bytes,resp_content_bytes,conn_id\n"
            forM_ allents $ \Entry {..} -> do
                hPrintf h "%.6f,%.4f,%.4f,%.4f,%.4f,%d,%u,%u,%u\n"
                    entConnect
                    (1000 * (entReqStart  - entConnect))
                    (1000 * (entReqDone   - entReqStart))
                    (1000 * (entRespStart - entReqDone))
                    (1000 * (entRespDone  - entRespStart))
                    entRespCode
                    entRespRLen
                    entRespCLen
                    entConnId

    ----------------------------------------------------------------------------

    return ()
  where
    avg :: [Double] -> Double
    avg xs = sn / fromIntegral ln
      where
        (sn,ln) = foldl' go (0,0::Int) xs
        go (s,l) x = (s+x,l+1)

doReqs :: SockAddr -> Word -> IORef Int -> ByteString -> IO [Entry]
doReqs sa idx cntref req = do
    t0 <- getTS
    ss <- ssConnect sa
    es <- go ss (Just t0) []
    ssClose ss
    return es
  where
    go ss mt0 a = do
        isc <- sIsConnected (ssToSocket ss)
        notDone <- decReqCounter cntref
        if notDone && isc
        then do
            r <- doReq ss idx req mt0
            go ss Nothing (r:a)
        else
            return a

    decReqCounter cnt = atomicModifyIORef' cnt (\n -> (n-1,n>0))

doReq :: SockStream -> Word -> ByteString -> Maybe TS -> IO Entry
doReq ss i rawreq mt0 = do
    t1  <- getTS -- before request is sent (and socket is already connected)
    ssWrite rawreq ss
    t1' <- getTS -- after request has been sent

    _ <- ssPeek ss
    t2  <- getTS -- after first packet has been received

    rcnt0 <- ssReadCnt ss

    hds <- recvHttpHeaders ss
    let (code, needClose, clen) = httpHeaderGetInfos hds

    clen' <- case clen of
        Just n | n < 0     -> fail "invalid response w/ unknown content-length"
               | otherwise -> goClenBody n
        Nothing            -> goChunkedBody

    t2'  <- getTS
    rcnt1 <- ssReadCnt ss

    -- sanity check
    tmp <- ssPeekBuf ss
    unless (B.null tmp) $
        fail "trailing garbage detected in server response -- aborting"

    when needClose $ do
        putStrLn "server requested connection-close!"
        ssClose ss

    return $! Entry i code (rcnt1-rcnt0) clen' (fromMaybe t1 mt0) t1 t1' t2 t2'

  where
    goClenBody :: Int -> IO Word64
    goClenBody n0
      | n0 >= 0   = do
          _ <- ssRead' ss (fromIntegral n0)
          return $ fromIntegral n0
      | otherwise = fail "goClenBody: negative content-length size"

    goChunkedBody :: IO Word64
    goChunkedBody = go' 0
      where
        go' n = do
            csz <- consumeChunk B.empty
            if csz > 0
            then go' (n+csz)
            else return n

        stripCR = fst . B8.spanEnd (=='\r')

        readHex bs | Just (n,rest) <- readHexadecimal bs, B.null rest, n>=0 = Just n
                   | otherwise = Nothing

        dropCrLf = do
            tmp <- ssRead' ss 2
            unless (tmp == "\r\n") $ fail "dropCrLf: expected CRLF"

        consumeChunk :: ByteString -> IO Word64
        consumeChunk buf
            | Just j <- B.elemIndex 10 buf = do
                let (chunksize',rest) = (stripCR $ B.unsafeTake j buf, B.unsafeDrop (j+1) buf)
                ssUnRead rest ss
                chunksize <- maybe (fail "invalid chunk-size") return $ readHex chunksize'
                _ <- ssRead' ss chunksize
                dropCrLf
                return $! fromIntegral chunksize
            | otherwise = do -- no '\n' seen yet... need more data
                buf' <- ssRead ss
                consumeChunk (buf<>buf')
