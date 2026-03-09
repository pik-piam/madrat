# getNonDefaultArguments

Given a function and an argument list, identify which arguments are
different from their default.

## Usage

``` r
getNonDefaultArguments(functionName, args, errorOnMismatch = TRUE)
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

A subset of args that is used by the function/s and is different from
default values.

## See also

[`cacheArgumentsHash`](cacheArgumentsHash.md),
[`toolstartmessage`](toolstartmessage.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
madrat:::getNonDefaultArguments("madrat:::readTau", args = list(subtype = "historical"))
#> $subtype
#> [1] "historical"
#> 
madrat:::getNonDefaultArguments("madrat:::readTau", args = list(subtype = "paper"))
#> NULL
functionNames <- c(madrat:::readTau, madrat:::convertTau)
madrat:::getNonDefaultArguments(functionNames, args = list(subtype = "historical"))
#> $subtype
#> [1] "historical"
#> 
```
