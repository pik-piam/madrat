# Tool: cacheArgumentsHash

Calculate hash from given function arguments for given call

## Usage

``` r
cacheArgumentsHash(functionName, args = NULL, errorOnMismatch = TRUE)
```

## Arguments

- functionName:

  A function name as a string (usually in the form package:::fun, e.g.
  madrat:::calcTauTotal). Passing a vector of functions is possible, but
  is only intended for corresponding read/correct/convert functions. If
  multiple functions in a vector define arguments with the same name but
  different default values only the default defined in the first
  function is considered.

- args:

  A list of named arguments used to call the given function(s). If
  duplicates of arguments exists the first occurrence of the argument
  will be used.

- errorOnMismatch:

  Whether an error is thrown in case an argument in args is not accepted
  by functionName.

## Value

A hash representing the given arguments hash for the given call. NULL,
if no argument deviates from the default argument settings.

## See also

[`cachePut`](cachePut.md), [`cacheName`](cacheName.md),
[`getNonDefaultArguments`](getNonDefaultArguments.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
madrat:::cacheArgumentsHash("madrat:::readTau", args = list(subtype = "historical"))
#> 
#> Initializing madrat config with default settings
#> Temporary main folder will be used..
#>     regionmapping = regionmappingH12.csv
#>     extramappings = NULL
#>     packages = madrat
#>     globalenv = FALSE
#>     verbosity = 1
#>     mainfolder = /tmp/RtmpbTZcVa/madrat
#>     sourcefolder = NA
#>     cachefolder = NA
#>     mappingfolder = NA
#>     outputfolder = NA
#>     pucfolder = NA
#>     tmpfolder = NA
#>     nolabels = NULL
#>     forcecache = FALSE
#>     ignorecache = NULL
#>     cachecompression = gzip
#>     hash = xxhash32
#>     diagnostics = FALSE
#>     debug = FALSE
#>     maxLengthLogMessage = 200
#> ..done!
#> [1] "-50d72f51"
madrat:::cacheArgumentsHash("madrat:::readTau", args = list(subtype = "paper"))
#> NULL
functionNames <- c("madrat:::readTau", "madrat:::convertTau")
madrat:::cacheArgumentsHash(functionNames, args = list(subtype = "historical"))
#> [1] "-50d72f51"
```
