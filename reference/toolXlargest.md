# toolXlargest

Selects the countries with the highest values in a magpie object

## Usage

``` r
toolXlargest(x, range = 1:20, years = NULL, elements = NULL)
```

## Arguments

- x:

  magclass object that shall be used for ranking

- range:

  the position of the countries in the top X which should be returned.

- years:

  range of years that shall be summed for ranking. If NULL, the sum of
  all years is used.

- elements:

  range of elements that shall be summed for ranking. If NULL, all
  elements are used.

## Value

vector with ISO country codes

## Author

Benjamin Leon Bodirsky, Jan Philipp Dietrich

## Examples

``` r
toolXlargest(magclass::maxample("pop"), range = 1:3)
#> [1] "SAS" "AFR" "CPA"
```
