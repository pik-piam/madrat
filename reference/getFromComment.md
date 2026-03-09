# getFromComment

Helper function extract a metadata comment

## Usage

``` r
getFromComment(x, name)
```

## Arguments

- x:

  object the metadata should be extracted from

- name:

  name of the metadata to be extracted (e.g. unit)

## Author

Jan Philipp Dietrich

## Examples

``` r
x <- as.magpie(1)
getComment(x) <- c(" description: example description", " unit: kg")
getFromComment(x, "unit")
#> [1] "kg"
getFromComment(x, "description")
#> [1] "example description"
```
