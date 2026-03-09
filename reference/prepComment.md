# prepComment

Helper function to properly format a metadata comment entry

## Usage

``` r
prepComment(x, name, warning = NULL)
```

## Arguments

- x:

  content to be added as metadata comment

- name:

  Name of the metadata entry

- warning:

  Either NULL (no warning) or a warning text that should be returned if
  x is NULL

## Author

Jan Philipp Dietrich

## Examples

``` r
 madrat:::prepComment("example comment", "example")
#> [1] " example: example comment"
```
