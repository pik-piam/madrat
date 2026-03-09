# Tool: NA replace

Functions removes NAs, NaNs and infinite values in x and weight

## Usage

``` r
toolNAreplace(x, weight = NULL, replaceby = 0, val.rm = NULL)
```

## Arguments

- x:

  data

- weight:

  aggregation weight

- replaceby:

  value which should be used instead of NA. Either a single value or a
  MAgPIE object which can be expanded to the size of x (either same size
  or with lower dimensionality).

- val.rm:

  vector of values that should in addition be removed in x

## Value

a list containing x and weight

## See also

[`calcOutput`](calcOutput.md)

## Author

Benjamin Bodirsky, Jan Philipp Dietrich
