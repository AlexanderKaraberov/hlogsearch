# hlogsearch

A minimalistic tool written in Haskell allows for searching log entries in arbitrarily large and noisy log files by specifying a searchable log datetime and precision. It can be used either as a standalone CLI tool, as a helper library to build sophisticated log analysis tools, or simply as the first step in a larger UNIX pipeline.

## Usage

Search for a log entry using a given timestamp with millisecond precision:

```sh
hlogsearch -p /var/log/cassandra/gc.log -d 2019-12-12T10:34:28.909343455Z --precision 0.001 
```

Note: precision of 1 means 1 second.

## Build

`cabal run` is all you need.

Refer to `Main.hs` for an example on how to use `hlogsearch` as a library. 

`hlogsearch` expects unstructured log file formats with dates printed in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
