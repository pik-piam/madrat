# getSources

These functions can be used to retrieve a list of currently available
sources and outputs (based on the availability of corresponding
conversion functions in the loaded data data processing packages.)

## Usage

``` r
getSources(
  name = NULL,
  type = NULL,
  packages = getConfig("packages"),
  globalenv = getConfig("globalenv")
)
```

## Arguments

- name:

  name of function for which sources should get returned. If not
  specified, all sources in the specified environment are returned

- type:

  Type of source, either set to "read", "convert", "correct", "download"
  or NULL. If specified, a vector containing the sources with the
  corresponding function type are returned, otherwise a data.frame with
  all sources and their available function types is returned.

- packages:

  A character vector with packages for which the available
  Sources/Calculations should be returned

- globalenv:

  Boolean deciding whether sources/calculations in the global
  environment should be included or not

## Value

A vector or data.frame containing all corresponding sources

## Note

Please be aware that these functions only check the availability of
corresponding functions of the package, not whether the functions will
properly work.

## See also

[`readSource`](readSource.md), [`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
print(getSources())
#>   source read correct convert download
#> 1    Tau TRUE   FALSE    TRUE     TRUE
```
