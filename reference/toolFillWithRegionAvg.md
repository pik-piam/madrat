# Tool: FillWithRegionAvg

This function fills missing values for countries with the (weighted)
average of the respective region. The average is computed separately for
every timestep. Currently only inputs with one data dimension are
allowed as inputs. (If the filling should be performed over multiple
data dimensions, call this function multiple times and bind the results
together with magclass::mbind.)

## Usage

``` r
toolFillWithRegionAvg(
  x,
  valueToReplace = NA,
  weight = NULL,
  callToolCountryFill = FALSE,
  regionmapping = NULL,
  verbose = TRUE,
  warningThreshold = 0.5,
  noteThreshold = 1
)
```

## Arguments

- x:

  MAgPIE object with country codes in the first and time steps in the
  second dimension.

- valueToReplace:

  value that denotes missing data. Defaults to NA.

- weight:

  MAgPIE object with weights for the weighted average. Must contain at
  least all the countries and years present in x. If no weights are
  specified, an unweighted average is performed.

- callToolCountryFill:

  Boolean variable indicating whether the list of countries should first
  be filled to the official ISO code country list. Subsequently the
  newly added and previously missing values are filled with the region
  average.

- regionmapping:

  Data frame containing the mapping between countries and regions.
  Expects column names CountryCode and RegionCode. Uses the currently
  set mapping if no mapping is specified.

- verbose:

  Boolean variable indicating if the function should print out what it
  is doing. Can generate a lot of output for a large object.

- warningThreshold:

  If more than this fraction of the countries in a given region and
  timestep have a missing value, throw a warning.

- noteThreshold:

  If more than this fraction of the countries in a given region and
  timestep have a missing value, a note will be written.

## Value

A MAgPIE object with the missing values filled.

## Details

toolFillWithRegionAvg can be used in conjunction with toolCountryFill()
to first fill up the list of countries to the official ISO code country
list, and then fill values with the regional average (see
callToolCountryFill Option).

## Author

Bjoern Soergel, Lavinia Baumstark, Jan Philipp Dietrich

## Examples

``` r
x <- magclass::new.magpie(cells_and_regions = c("A", "B", "C", "D"), years = c(2000, 2005),
  fill = c(1, NA, 3, 4, 5, 6, NA, 8))
rel <- data.frame(CountryCode = c("A", "B", "C", "D"), RegionCode = c("R1", "R1", "R1", "R2"))
xfilled <- toolFillWithRegionAvg(x, regionmapping = rel)
#> Replaced missing values with regional average for: R1|1 (1x) -> 2, R1|2 (1x) -> 5.5
```
