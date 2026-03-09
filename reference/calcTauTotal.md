# Calculate total tau

This function prepares total tau values for use. As the source data
already provides all required information this function purely removes
not required data and moves xref values to the weighting object which is
required for aggregation.

## Usage

``` r
calcTauTotal(source = "paper")
```

## Arguments

- source:

  data source, either "paper" (default) or "historical".

## Value

Total tau data and corresponding weights as a list of two MAgPIE objects

## See also

[`calcOutput`](calcOutput.md), [`readTau`](readTau.md),
[`convertTau`](convertTau.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("TauTotal")
} # }
```
