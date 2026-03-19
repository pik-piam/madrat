# prepFunctionName

Function to prepare a function call for a given type and prefix

## Usage

``` r
prepFunctionName(type, prefix = "calc", ignore = NULL, error_on_missing = TRUE)
```

## Arguments

- type:

  name of calculation/source

- prefix:

  Type of calculations. Available options are "download" (source
  download), "read" (source read), "correct" (source corrections),
  "convert" (source conversion to ISO countries), "calc" (further
  calculations), and "full" (collections of calculations)

- ignore:

  vector of arguments which should be ignored (not be part of the
  function call)

- error_on_missing:

  boolean deciding whether a missing type should throw an error or
  return NULL

## Value

A function call as character to the specified function with
corresponding package as attribute

## See also

[`readSource`](readSource.md), [`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
print(madrat:::prepFunctionName("Tau", "read"))
#> [1] "madrat:::readTau(subtype=subtype)"
#> attr(,"formals")
#> [1] "subtype"
#> attr(,"package")
#> [1] "madrat"
#> attr(,"pkgcomment")
#> [1] "madrat 3.36.1"
print(madrat:::prepFunctionName("TauTotal", "calc"))
#> [1] "madrat:::calcTauTotal(...)"
#> attr(,"formals")
#> character(0)
#> attr(,"package")
#> [1] "madrat"
#> attr(,"pkgcomment")
#> [1] "madrat 3.36.1"
print(madrat:::prepFunctionName("EXAMPLE", "full"))
#> [1] "madrat:::fullEXAMPLE(rev=rev, dev=dev, ...)"
#> attr(,"formals")
#> [1] "rev" "dev"
#> attr(,"package")
#> [1] "madrat"
#> attr(,"pkgcomment")
#> [1] "madrat 3.36.1"
```
