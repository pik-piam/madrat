# getMadratGraph

Function returns the madrat graph of all linkages of full, calc, and
read functions of the given madrat based packages. Linkages to
subfunctions of read functions (i.e. download, correct or convert
functions) are not listed separately, but collectively referred to
through the corresponding read function.

## Usage

``` r
getMadratGraph(
  packages = installedMadratUniverse(),
  globalenv = getConfig("globalenv")
)
```

## Arguments

- packages:

  A character vector with packages for which the available
  Sources/Calculations should be returned

- globalenv:

  Boolean deciding whether sources/calculations in the global
  environment should be included or not

## Value

A data frame with 4 columns: from (source function), from_package
(package the source function originates from), to (function which is
using the source), to_package (package of the using function)

## See also

[`getCalculations`](getCalculations.md), [`getConfig`](getConfig.md)

## Author

Jan Philipp Dietrich
