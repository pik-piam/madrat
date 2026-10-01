# findMemoryBottlenecks

Analyzes a log from a retrieveData run for which
`setConfig(memoryProfiling = TRUE)` was active, and identifies which
processing stages drive the memory requirement of the run.

## Usage

``` r
findMemoryBottlenecks(file, unit = "MB", cumulative = TRUE)
```

## Arguments

- file:

  path to a log file or content of a log as character vector

- unit:

  unit for memory information, either "MB" (megabytes) or "GB"
  (gigabytes)

- cumulative:

  boolean deciding whether calls to the same function should be
  aggregated or not

## Value

A named list with one entry per retrieveData call found in the log, plus
a "standalone" entry collecting all calls that do not belong to any
retrieveData call (e.g. calcOutput/readSource calls made directly from a
script). The names are the retrieveData types (or "standalone") and each
entry is a data.frame sorted by peak memory usage, showing for the
different data processing functions their peak memory usage "peak" (the
highest resident set size seen during the stage), their share of the
block's peak, and their memory "growth" (the change in resident set size
from before to after the stage, summed across calls of the same type if
`cumulative = TRUE`). If `cumulative = FALSE` the usage before ("start")
and after ("end") each individual call is reported as well.

## See also

[`setConfig`](setConfig.md), [`findBottlenecks`](findBottlenecks.md)

Other analysis: [`findBottlenecks()`](findBottlenecks.md)

## Author

Patrick Rein
