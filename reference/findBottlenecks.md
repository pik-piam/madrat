# findBottlenecks

Analyzes a log from a retrieveData run or from a script calling
calcOutput()/readSource() directly, extracts runtime information for all
called functions and identifies most critical bottlenecks.

## Usage

``` r
findBottlenecks(file, unit = "min", cumulative = TRUE)
```

## Arguments

- file:

  path to a log file or content of a log as character vector

- unit:

  unit for runtime information, either "s" (seconds), "min" (minutes) or
  "h" (hours)

- cumulative:

  boolean deciding whether calls to the same function should be
  aggregated or not

## Value

A named list with one entry per retrieveData call found in the log, plus
a "standalone" entry collecting all calls that do not belong to any
retrieveData call (e.g. calcOutput/readSource calls made directly from a
script). The names are the retrieveData types (or "standalone") and each
entry is a data.frame sorted by net runtime showing for the different
data processing functions their total runtime "time" (including all
sub-functions) and net runtime "net" (excluding sub-functions) and their
share of total runtime. For the "standalone" entry the total runtime is
the sum of its top-level call runtimes, not wall clock time, as it
excludes any non-madrat time between those calls.

## Author

Jan Philipp Dietrich
