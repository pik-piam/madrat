# cleanComment

Helper function to clean a comment from additional metadata information

## Usage

``` r
cleanComment(
  x,
  remove = c("unit", "description", "comment", "origin", "creation date", "note")
)
```

## Arguments

- x:

  magclass object the comment should be read from

- remove:

  Vector of categories to be removed

## Author

Jan Philipp Dietrich

## Examples

``` r
x <- maxample("animal")
getComment(x) <- c("unit: bla", "comment: hallo", "blub: ble")
madrat:::cleanComment(x)
#> [1] "blub: ble"
```
