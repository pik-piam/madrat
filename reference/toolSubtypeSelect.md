# Tool: SubtypeSelect

This function is a support function for the selection of a subtype in a
readX function. In addition to the subtype selection it also performs
some consistency checks.

## Usage

``` r
toolSubtypeSelect(subtype, files)
```

## Arguments

- subtype:

  A chosen subtype (character)

- files:

  A named vector or list. The names of the vector correspond to the
  allowed subtypes and the content of the vector are the corresponding
  file names.

## Value

The file name corresponding to the given subtype

## See also

[`readSource`](readSource.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
files <-  c(protection="protection.csv",
              production="production.csv",
              extent="forest_extent.csv")
toolSubtypeSelect("extent",files)
#> [1] "forest_extent.csv"
```
