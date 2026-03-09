# toolFixWeight

Check the example below for the problem this function is solving.
Instead of adding a small number (like 10^-10) to each entry in weight,
set only those weights to 10^-30 where otherwise the total sum of the
(dis)aggregated object would be different from the original.

## Usage

``` r
toolFixWeight(weight, map, dim)
```

## Arguments

- weight:

  magclass object containing weights to be used for a weighted
  (dis)aggregation. The provided weight does not need to be normalized,
  any number \>= 0 is allowed.

- map:

  a data frame where the first column contains coarse resolution items
  and the second column contains fine resolution items; fine resolution
  items must match items in weight

- dim:

  which dim to fix (e.g. 1 or 3.2 or "region")

## Value

weight, with weights set to 10^-30 only where otherwise the total sum of
the (dis)aggregated object would be different from the original

## See also

[`toolAggregate`](toolAggregate.md)

## Author

Pascal Sauer

## Examples

``` r
x <- new.magpie(c("A", "B"), fill = 100)
rel <- data.frame(c("A", "A", "B", "B"),
                  c("A1", "A2", "B1", "B2"))
weight <- new.magpie(c("A1", "A2", "B1", "B2"), fill = 0)
weight["B1", , ] <- 1

y <- toolAggregate(x, rel, weight) # warning "Weight sum is 0"
#> Warning: Weight sum is 0, so cannot normalize and will return 0 for some aggregation targets. This changes the total sum of the magpie object! If this is really intended set zeroWeight = "allow", or "setNA" to return NA.
stopifnot(sum(x) - sum(y) == 100) # total sum no longer equal, hence the warning
stopifnot(as.vector(y["B2", , ]) == 0)

# problematic hotfix
y <- toolAggregate(x, rel, weight + 10^-10) # no warning
stopifnot(sum(x) == sum(y)) # total sum is equal
stopifnot(as.vector(y["B2", , ]) > 0) # this is the problem, should still be 0

# proper fix
y <- toolAggregate(x, rel, weight, zeroWeight = "fix") # no warning
stopifnot(sum(x) == sum(y)) # total sum is equal
stopifnot(as.vector(y["B2", , ]) == 0) # and this is also still 0
```
