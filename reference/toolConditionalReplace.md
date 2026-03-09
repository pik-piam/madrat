# toolConditionalReplace

Sets values (NA, negative, ..) to value `replaceby`

## Usage

``` r
toolConditionalReplace(x, conditions, replaceby = 0)
```

## Arguments

- x:

  magpie object

- conditions:

  vector of conditions for values, that should be removed e.g.
  "is.na()", "\< 0" (order matters)

- replaceby:

  value which should be used instead (can be a vector of same length as
  conditions as well)

## Value

return changed input data

## Author

Kristine Karstens
