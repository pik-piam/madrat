# toolOrderCells

reorder numbered spatial units (cells, clusters) by number. Function
will return the unmodified object, if the given subdimension does not
exist or does not contain cell information.

## Usage

``` r
toolOrderCells(x, dim = 1.2, na.rm = FALSE)
```

## Arguments

- x:

  magclass object that should be ordered

- dim:

  subdimension which contains the cell information

- na.rm:

  boolean deciding how to deal with non-integer information in cellular
  column. If FALSE, non-integer values will lead to a return of the
  unsorted object, if TRUE non-integer cells will be removed from the
  data set and the rest will get sorted

## Value

ordered data in magclass format

## Author

Kristine Karstens, Jan Philipp Dietrich
