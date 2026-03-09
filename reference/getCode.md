# getCode

Extract function code from madrat-style functions in specified packages

## Usage

``` r
getCode(
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

A named vector with condensed function code

## See also

[`getMadratGraph`](getMadratGraph.md)

## Author

Jan Philipp Dietrich
