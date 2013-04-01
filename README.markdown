# `uhttpc` - µHTTP client library

`uhttpc` is a simple low-level and lightweight Haskell HTTP 1.1
library providing the bare minimum required for HTTP benchmarking
purposes.

This is **not** a RFC compliant HTTP client library and **shall not**
be used as a general purpose HTTP implementation!

# `uhttpc-bench`

This Cabal package comes with an executable `uhttpc-bench` which
represents an `ab`/`weighttpd`-style HTTP benchmarking tool:

    uhttpc-bench - a Haskell-based ab/weighttp-style webserver benchmarking tool

    Simple HTTP benchmark tool similiar to ab and weighttp

    uttpc-bench [OPTIONS] <url>

    Common flags:
      -n=num                     number of requests    (default: 1)
      -t=num                     threadcount           (default: 1)
      -c=num                     concurrent clients    (default: 1)
      -k                         enable keep alive
              --csv=FILE         dump request timings as CSV (RFC4180) file
              --user-agent=ITEM  specify User-Agent    (default: "httpc-bench")
      -H=str                     add header to request
      -v      --verbose          enable more verbose statistics and output
              --no-stats         disable statistics
      -p=FILE                    perform POST request with file-content as body
      -?      --help             Display help message
      -V      --version          Print version information

# How to use it

First, clone and build/install
(This requires GHC 7.6.x or a GHC 7.7.x snapshot):

    $ git clone git://github.com/hvr/uhttpc.git
    $ cd uhttpc
    $ cabal install

And example invocation of `uhttpc-bench`:

    $ uhttpc-bench http://www.google.com/ -v -c8 -n1000

results in the following output:

    uhttpc-bench - a Haskell-based ab/weighttp-style webserver benchmarking tool

    using 88-byte request header (+ 0-byte body):
     "GET / HTTP/1.1\r\nHost: www.google.com:80\r\nUser-Agent: uhttpc-bench\r\nConnection: close\r\n\r\n"

    starting benchmark...

    per-client stats:

     client spawned +0.000008 s, 125 reqs (125 conns), 8.1 req/s, finished in 15.369619 s
     rtt min/avg/med/max = 99.586/122.902/122.109/167.193 ms

     client spawned +0.000258 s, 125 reqs (125 conns), 8.1 req/s, finished in 15.383278 s
     rtt min/avg/med/max = 100.494/123.012/121.648/184.555 ms

     client spawned +0.000292 s, 125 reqs (125 conns), 8.1 req/s, finished in 15.365843 s
     rtt min/avg/med/max = 98.721/122.872/121.668/163.810 ms

     client spawned +0.000327 s, 124 reqs (124 conns), 8.1 req/s, finished in 15.346788 s
     rtt min/avg/med/max = 103.008/123.709/122.834/174.003 ms

     client spawned +0.000366 s, 126 reqs (126 conns), 8.2 req/s, finished in 15.367205 s
     rtt min/avg/med/max = 95.255/121.907/120.736/152.943 ms

     client spawned +0.000403 s, 124 reqs (124 conns), 8.1 req/s, finished in 15.357957 s
     rtt min/avg/med/max = 97.730/123.800/123.569/162.326 ms

     client spawned +0.000434 s, 125 reqs (125 conns), 8.1 req/s, finished in 15.388717 s
     rtt min/avg/med/max = 103.056/123.055/121.961/162.419 ms

     client spawned +0.000461 s, 126 reqs (126 conns), 8.2 req/s, finished in 15.394365 s
     rtt min/avg/med/max = 102.114/122.123/121.289/151.520 ms

    finished in 15.394867 seconds, 1000 reqs (1000 conns), 65.0 req/s received
    status codes: 1000 HTTP-302
    data received: 63.498 KiB/s, 1001000 bytes total (783000 bytes http + 218000 bytes content)
    rtt 2/9|25/50/75|91/98-th quantile = 103.737/108.696 | 115.124/121.688/129.944 | 137.715/148.159 ms
    rtt min/avg/max = 95.255/122.919/184.555 ms


The `--csv` option allows to export the raw measurement data in format
suitable for offline analysis with statistical tools such as `R`.
