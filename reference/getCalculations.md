# getCalculations

This function can be used to retrieve a list of currently available
sources and outputs (based on the availability of corresponding
conversion functions in the loaded data data processing packages.)

## Usage

``` r
getCalculations(
  prefix = "calc",
  packages = getConfig("packages"),
  globalenv = getConfig("globalenv")
)
```

## Arguments

- prefix:

  Type of calculations, vector of types or search term (e.g.
  "read\|calc"). Available options are "download" (source download),
  "read" (source read), "correct" (source corrections), "convert"
  (source conversion to ISO countries), "calc" (further calculations),
  and "full" (collections of calculations)

- packages:

  A character vector with packages for which the available
  Sources/Calculations should be returned

- globalenv:

  Boolean deciding whether sources/calculations in the global
  environment should be included or not

## Value

A data frame containing all currently available outputs of all loaded
data processing packages including its name, its function call and its
package origin.

## See also

[`readSource`](readSource.md), [`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
print(getCalculations())
#>       type package                  call
#> 1 TauTotal  madrat madrat:::calcTauTotal
print(getCalculations("read"))
#>   type package             call
#> 1  Tau  madrat madrat:::readTau
```
