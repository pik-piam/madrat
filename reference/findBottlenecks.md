# findBottlenecks

Analyzes a log from a retrieveData run, extracts runtime information for
all called functions and identifies most critical bottlenecks.

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

A data.frame sorted by net runtime showing for the different data
processing functions their total runtime "time" (including the execution
of all sub-functions) and net runtime "net" (excluding the runtime of
sub-functions) and their share of total runtime.

## Author

Jan Philipp Dietrich
