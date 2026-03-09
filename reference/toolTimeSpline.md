# Smooth a magclass time series with optional anchor years

Smooths a magclass time series using spline approximation with the given
degrees of freedom. Optionally, specific years can be "pegged"
(anchored) to stay close to their original values during smoothing.
Anchoring is enforced by applying high weights to those years.

## Usage

``` r
toolTimeSpline(x, dof = 5, peggedYears = NULL, anchorFactor = 10)
```

## Arguments

- x:

  A magclass object.

- dof:

  Degrees-of-freedom per 100 years (higher -\> more degrees of freedom,
  less smoothing; default 5).

- peggedYears:

  Integer vector of years (e.g. \`c(2020, 2050, 2100)\`) to anchor
  during smoothing; NULL for none (default).

- anchorFactor:

  Numeric multiplier for anchor weights (default 10); larger values more
  strongly enforce pegging.

## Value

A magclass object of the same shape, with each time series
spline-smoothed.

## Author

Kristine Karstens, Felicitas Beier, Michael Crawford
