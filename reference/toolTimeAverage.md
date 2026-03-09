# toolTimeAverage

average over time given an averaging range. Only works for data with
equidistant time steps!

## Usage

``` r
toolTimeAverage(x, averaging_range = NULL, cut = TRUE)
```

## Arguments

- x:

  magclass object that should be averaged with equidistant time steps

- averaging_range:

  number of time steps to average

- cut:

  if TRUE, all time steps at the start and end that can not be averaged
  correctly, will be removed if FALSE, time steps at the start and end
  will be averaged with high weights for start and end points

## Value

the averaged data in magclass format

## Author

Kristine Karstens, Jan Philipp Dietrich
