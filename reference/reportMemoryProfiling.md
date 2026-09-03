# reportMemoryProfiling

Ends a memory profiling block started by
[`startMemoryProfiling`](startMemoryProfiling.md) and writes the
collected numbers to the log.

## Usage

``` r
reportMemoryProfiling(start, callString)
```

## Arguments

- start:

  the baseline as returned by
  [`startMemoryProfiling`](startMemoryProfiling.md); if NULL nothing is
  reported

- callString:

  the function call the measurement belongs to

## See also

[`startMemoryProfiling`](startMemoryProfiling.md)

## Author

Patrick Rein
