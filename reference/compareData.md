# compareData

Compares the content of two data archives and looks for similarities and
differences

## Usage

``` r
compareData(x, y, tolerance = 10^-5, yearLim = NULL, detailed = FALSE)
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

- detailed:

  if TRUE, files that differ are additionally broken down into
  storage/reassociation noise, zero-flips (one side exactly 0, the other
  dust – see the "zero-flip" note in the printed report), and genuine
  differences. This is purely diagnostic: it doesn't change the OK/DIFF
  verdict.

## Value

Invisibly, a list with the ok/skip/diff/miss counts, the file lists, and
(if `detailed = TRUE`) a `details` list of per-file difference
statistics keyed by file name.

## See also

[`setConfig`](setConfig.md), [`calcTauTotal`](calcTauTotal.md),

Other validation: [`compareMadratOutputs()`](compareMadratOutputs.md),
[`toolCompareStatusLogs()`](toolCompareStatusLogs.md)

## Author

Jan Philipp Dietrich, Florian Humpenoeder
