# fullExample

Example for class of fullX functions. Can be used as template for a new
function or for testing the basic functionality

## Usage

``` r
fullEXAMPLE(rev = 0, dev = "", extra = "Example argument")
```

## Arguments

- rev:

  data revision which should be used/produced. Will be converted to
  [`numeric_version`](https://rdrr.io/r/base/numeric_version.html) when
  called via [`retrieveData`](retrieveData.md).

- dev:

  development suffix to distinguish development versions for the same
  data revision. This can be useful to distinguish parallel lines of
  development.

- extra:

  additional argument which - when changed - does not require a
  re-computation of the portable unaggegrated collection (puc) file.
  [`setConfig`](setConfig.md) (e.g. for setting the mainfolder if not
  already set properly).

## See also

[`readSource`](readSource.md),[`getCalculations`](getCalculations.md),[`calcOutput`](calcOutput.md),[`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
retrieveData("example", rev = "2.1.2", dev = "test", regionmapping = "regionmappingH12.csv")
} # }
```
