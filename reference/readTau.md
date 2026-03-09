# Read Tau

Read-in landuse intensity data (tau) following the methodology published
in Dietrich J.P., Schmitz C., Mueller C., Fader M., Lotze-Campen H.,
Popp A., Measuring agricultural land-use intensity - A global analysis
using a model-assisted approach, Ecological Modelling, Volume 232, 10
May 2012, Pages 109-118, ISSN 0304-3800,
10.1016/j.ecolmodel.2012.03.002.

## Usage

``` r
readTau(subtype = "paper")
```

## Arguments

- subtype:

  Type of Tau data that should be read. Available types are:

  - `paper`: numbers as they are reported in the paper (cellular,
    crop-specific)

  - `historical`: historic tau values on iso country level for total tau
    factor. This numbers were calculated by taking FAO yields and
    norming it to the 1995 tau values of the paper
    (faoyields\*tau95/mean(faoyields\[1995:2005\]))

## Value

Tau data and weights as MAgPIE object in original resolution

## See also

[`readSource`](readSource.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("Tau")
} # }
```
