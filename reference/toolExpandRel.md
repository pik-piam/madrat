# toolExpandRel

Expands aggregation matrix to full items for matrices that only contain
subdim items in from/to.

## Usage

``` r
toolExpandRel(rel, x, dim)
```

## Arguments

- rel:

  The aggregation matrix with subdim items

- x:

  The data to be aggregated already sub-set in case of a partial
  relation

- dim:

  The (sub-)dim to be aggregated

## Value

An expanded aggregation matrix

## Details

Works by first generating all possible from/to items, then sub-setting
to only the ones that are actually valid, i.e. from-items that appear in
the original data, and to-items that only contain sub-items that also
occur in the from-items, so an aggregated item should only contain
sub-items that were also present in all from-items it was aggregated
from.
