{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards, BangPatterns #-}

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Data
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Word
import           GHC.Conc.Sync (numCapabilities,getNumProcessors)
import           Network.HTTP.MicroClient
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           System.Console.CmdArgs.Implicit
import           System.IO
import           System.Mem (performGC)
import           Text.Printf

-- |Timestamp in seconds since POSIX epoch
getTS :: IO TS
getTS = getPOSIXTimeSecs

type TS = Double

timeIO :: IO t -> IO (TS, TS, t)
timeIO act = do
    ts0 <- getTS
    res <- act
    tsd <- fmap (subtract ts0) getTS
    return $! tsd `seq` (ts0,tsd,res)

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

data Args = Args
    { argNumReq    :: Word
    , argThrCnt    :: Int
    , argClnCnt    :: Word
    , argKeepAlive :: Bool
    , argCsv       :: FilePath
    , argUA        :: String
    , argHdrs      :: [String]
    , argVerbose   :: Bool
    , argNoStats   :: Bool
    , argLAddr     :: String
    , argWait      :: Double
    , argUrl       :: String
    , argPostFn    :: FilePath
    } deriving (Show,Data,Typeable)

defaultArgs :: Args
defaultArgs = Args
    { argNumReq    = 1 &= typ "num" &= name "n" &= explicit
                     &= help "number of requests    (default: 1)"
    , argThrCnt    = numCapabilities &= typ "num" &= explicit &= name "t"
                     &= help ("threadcount           (default: " ++ show numCapabilities ++ ")")
    , argClnCnt    = 1 &= typ "num" &= name "c" &= explicit
                     &= help "concurrent clients    (default: 1)"
    , argKeepAlive = def &= name "k" &= explicit
                     &= help "enable keep alive"
    , argHdrs      = def &= typ "str" &= name "H" &= explicit
                     &= help "add header to request"
    , argUA        = "uhttpc-bench" &= name "user-agent" &= explicit
                     &= help "specify User-Agent    (default: \"httpc-bench\")"
    , argCsv       = def &= typFile &= name "csv" &= explicit
                     &= help "dump request timings as CSV (RFC4180) file"
    , argVerbose   = def &= name "v" &= name "verbose" &= explicit
                     &= help "enable more verbose statistics and output"
    , argNoStats   = def &= name "no-stats" &= explicit
                     &= help "disable statistics"
    , argPostFn    = def &= typFile &= name "p" &= explicit
                     &= help "perform POST request with file-content as body"
    , argLAddr     = def &= typ "addr" &= name "local-addr" &= explicit
                     &= help "set specific source address for TCP connections"
    , argWait      = def &= typ "sec" &= name "wait" &= explicit
                     &= help "wait time between requests (per client) in seconds"
    , argUrl       = def &= argPos 0 &= typ "<url>"
    } &= program "uttpc-bench"
      &= summary "Simple HTTP benchmarking tool similiar to 'ab' and 'weighttp'"


main :: IO ()
main = runInUnboundThread $ do
    putStrLn "uhttpc-bench - a Haskell-based ab/weighttp-style webserver benchmarking tool\n"

    pargs <- cmdArgs defaultArgs
    let verbose = argVerbose pargs

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
    lsa <- if null (argLAddr pargs)
           then return Nothing
           else fmap Just $ getSockAddr (argLAddr pargs) aNY_PORT

    when verbose $
        printf "connecting to %s (using local address %s)\n" (show sa) (maybe "*:*" show lsa)

    -- set up worker threads
    when (fI (argThrCnt pargs) /= numCapabilities) $
        setNumCapabilities (fI $ argThrCnt pargs)
    thrcnt' <- getNumCapabilities

    when (argThrCnt pargs > 1 && thrcnt' /= argThrCnt pargs) $
        fail "failed to set worker tread-count"

    numCpus <- getNumProcessors

    when (thrcnt' > numCpus) $
        printf "*WARNING* thread-count (%u) > cpu cores (%u)\n" thrcnt' numCpus

    when (thrcnt' > 1) $
        printf "using up to %u Haskell execution threads\n" thrcnt'

    unless (fI (argClnCnt pargs) >= thrcnt') $
        putStrLn "*WARNING* client-count should be >= thread-count"

    ----------------------------------------------------------------------------
    let isPost = not $ null (argPostFn pargs)

    rawreqb <- if isPost
               then fmap Just $ B.readFile (argPostFn pargs)
               else return Nothing

    let hdrs | null (argUA pargs) = map B8.pack (argHdrs pargs)
             | otherwise          = "User-Agent: " <> B8.pack (argUA pargs) :
                                    map B8.pack (argHdrs pargs)

        rawreqblen = maybe 0 B.length rawreqb
        rawreq = mkHttp11Req (if isPost then POST else GET)
                             (B8.pack urlpath)
                             ("Host: " <> B8.pack hostname <> ":" <> B8.pack (show portnum))
                             (argKeepAlive pargs)
                             hdrs
                             rawreqb

    when verbose $
        printf "using %d-byte request message (%d-byte body):\n %s\n\n" (B.length rawreq) rawreqblen (show rawreq)

    putStrLn "starting benchmark..."

    reqCounter <- newIORef (fI (argNumReq pargs) :: Int)

    performGC
    ts0' <- getTS
    tss  <- mapConcurrently (\() -> timeIO (doReqs lsa sa reqCounter rawreq (argWait pargs)))
                            (replicate (fI $ argClnCnt pargs) ())
    ts1' <- getTS
    performGC

    numReqs' <- readIORef reqCounter
    unless (numReqs' + fI (argClnCnt pargs) == 0) $
        fail "virtual clients exited prematurely!"

    unless (argNoStats pargs) $ do
        when verbose $ do
            putStrLn "\nper-client stats:\n"

            forM_ tss $ \(ts0,tdelta,ents') -> do
                let n' = length ts'
                    ts' = map (\e -> entRespDone e - entConnect e) ents'
                    conncnt = length [ () | Entry {..} <- ents', entConnect /= entReqStart ]

                _ <- printf " client spawned +%.6f s, %d reqs (%d conns), %.1f req/s, finished in %.6f s\n"
                            (ts0-ts0') n' conncnt (fI n' / tdelta) tdelta

                unless (null ts') $
                    printf " rtt min/avg/med/max = %.3f/%.3f/%.3f/%.3f ms\n"
                           (1000*minimum ts') (1000*avg ts') (1000*med ts') (1000*maximum ts')
                putStrLn ""

        let allents = concatMap (\(_,_,ents) -> ents) tss
            (rlen,clen) = foldl' (\(!r,!c) e -> (r + entRespRLen e,c + entRespCLen e)) (0,0) allents
            conncnt = length [ () | Entry {..} <- allents, entConnect /= entReqStart ]
            allts = [ entRespDone - entConnect | Entry {..} <- allents ]
            statcodes = hist $ map entRespCode allents

        _ <- printf "finished in %.6f seconds, %d reqs (%d conns), %.1f req/s received\n"
                    (ts1'-ts0') (length allents) conncnt (fI (argNumReq pargs) / (ts1'-ts0'))

        putStrLn $ "status codes: " ++ intercalate ", " [ show y ++ " HTTP-" ++ show x | (x,y) <- statcodes ]

        _ <- printf "data received: %.3f KiB/s, %u bytes total (%u bytes http + %u bytes content)\n"
                    (fI rlen / (1024 * (ts1'-ts0'))) rlen (rlen-clen) clen

        when verbose $ do
            let [q2,q9,q25,q50,q75,q91,q98] = summary7 (map (*1000) allts)
            printf "rtt 2/9|25/50/75|91/98-th quantile = %.3f/%.3f | %.3f/%.3f/%.3f | %.3f/%.3f ms\n" q2 q9 q25 q50 q75 q91 q98

        printf "rtt min/avg/max = %.3f/%.3f/%.3f ms\n"
               (1000*minimum allts) (1000*avg allts) (1000*maximum allts)

    ----------------------------------------------------------------------------
    -- CSV dumping
    unless (null $ argCsv pargs) $ do
        putStrLn "dumping CSV..."
        let allents = sortBy (comparing entConnect) $ concatMap (\(_,_,ents) -> ents) tss
        withFile (argCsv pargs) WriteMode $ \h -> do
            _ <- hPrintf h "timestamp_s,connect_ms,req_send_ms,resp_wait_ms,resp_recv_ms,resp_status,resp_total_bytes,resp_content_bytes,conn_id\r\n"
            forM_ allents $ \Entry {..} ->
                hPrintf h "%.6f,%.4f,%.4f,%.4f,%.4f,%d,%u,%u,%d\r\n"
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
    avg xs = sn / fI ln
      where
        (sn,ln) = foldl' go (0,0::Int) xs
        go (!s,!l) x = (s+x,l+1)

    med :: [Double] -> Double
    med xs = sort xs !! (l `div` 2)
      where
        l = length xs

    summary7 :: [Double] -> [Double]
    summary7 xs = atIndices (sort xs) is
      where
        l = length xs
        is = [ ((l-1)*q) `div` 100 | q <- [2,9,25,50,75,91,98] ]

    atIndices :: [a] -> [Int] -> [a]
    atIndices _ []      = []
    atIndices [] _      = error "atIndices"
    atIndices xs (i:is) = head xs' : atIndices xs' (map (subtract i) is)
      where
        xs' | i >= 0 = drop i xs
            | otherwise = error "atIndices2"

    hist xs = [ (head g, length g) | g <- group (sort xs) ]

-- |repeat executing 'doReq' until req-counter goes below 0
doReqs :: Maybe SockAddr -> SockAddr -> IORef Int -> ByteString -> Double -> IO [Entry]
doReqs lsa sa cntref req wtime = go Nothing []
  where
    go ss a = do
        notDone <- decReqCounter cntref
        if notDone
        then do
            (ss',r) <- doReq lsa sa ss req
            when (wtimeUsecs > 0) $ threadDelay wtimeUsecs
            go ss' (r:a)
        else do
            maybe (return ()) ssClose ss
            return a

    wtimeUsecs = floor $ 1000000 * wtime

    decReqCounter cnt = atomicModifyIORef' cnt (\n -> (n-1,n>0))

-- |single measurement entry
data Entry = Entry
    { entConnId    :: {-# UNPACK #-} !Int -- each connections gets a new id
    , entRespCode  :: {-# UNPACK #-} !HttpCode
    , entRespRLen  :: {-# UNPACK #-} !Word64 -- total amount of data received
    , entRespCLen  :: {-# UNPACK #-} !Word64 -- payload content-length
    , entConnect   :: {-# UNPACK #-} !TS   -- ts at connection-startup
    , entReqStart  :: {-# UNPACK #-} !TS   -- ts before sending request
    , entReqDone   :: {-# UNPACK #-} !TS   -- ts after sending request
    , entRespStart :: {-# UNPACK #-} !TS   -- ts when response starts being received
    , entRespDone  :: {-# UNPACK #-} !TS   -- ts when full response has been received
    } deriving (Show)

doReq :: Maybe SockAddr -> SockAddr -> Maybe SockStream -> ByteString -> IO (Maybe SockStream,Entry)
doReq lsa sa mss rawreq = do
    (ss,mt0) <- case mss of
        Just ss' -> return (ss',Nothing)
        Nothing  -> do
            t0' <- getTS
            ss' <- ssConnect lsa sa
            return (ss',Just t0')

    t1  <- getTS -- before request is sent (and socket is already connected)
    ssWrite rawreq ss
    t1' <- getTS -- after request has been sent

    _ <- ssPeek ss
    t2  <- getTS -- after first packet of response has been received

    rcnt0 <- ssReadCnt ss

    -- read HTTP response
    HttpResponse {..} <- recvHttpResponse ss

    t2'  <- getTS -- after full response has been received
    rcnt1 <- ssReadCnt ss

    -- sanity check
    tmp <- ssPeekBuf ss
    unless (B.null tmp) $
        fail "trailing garbage detected in server response -- aborting"

    -- keep-alive handling
    ss' <- if respKeepalive
           then
               return (Just ss)
           else do
               ssClose ss
               return Nothing

    let ent = Entry (ssId ss) respCode (rcnt1-rcnt0) respContentLen (fromMaybe t1 mt0) t1 t1' t2 t2'

    return $! strictPair ss' ent
  where
    strictPair !a !b = (a,b)
