# compareData

Compares the content of two data archives and looks for similarities and
differences

## Usage

``` r
compareData(x, y, tolerance = 10^-5, yearLim = NULL)
```

## Arguments

- x:

  Either a tgz file or a folder containing data sets

- y:

  Either a tgz file or a folder containing data sets

- tolerance:

  tolerance level below which differences will get ignored

- yearLim:

  year until when the comparison should be performed. Useful to check if
  data is identical until a certain year.

## See also

[`setConfig`](setConfig.md), [`calcTauTotal`](calcTauTotal.md),

## Author

Jan Philipp Dietrich, Florian Humpenoeder
