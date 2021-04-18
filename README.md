# hlogsearch

A minimalistic tool written in Haskell to search log entries in arbitrary large and noisy log files by a very precise log datetime (up to 1 microsecond). It can be used either as a standalone tool, or as a helper library to build sophisticated log analysis tools, or even as a mere first step in your larger UNIX pipeline to help analyze logs such as:

```sh
hlogsearch "2021-03-14T14:10:03.000000" gc.log | grep -A 100 -B 100 | awk '{print $9}' | sort | uniq -c 
```

## Usage

`cabal run` is all you need.

Refer to `Main.hs` for an example on how to use this tool. `hlogsearch` expects unstructured log file formats with dates printed in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
