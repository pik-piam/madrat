# pucCreate

Creates a puc-file ("portable unaggregated collection") for a collection
that was already computed via [`retrieveData`](retrieveData.md) (e.g.
with `puc = FALSE`, or where a puc could not be created at the time, for
example due to strict mode). This makes it possible to create a puc-file
for an existing archive without having to rerun the underlying
calculations.

## Usage

``` r
pucCreate(archive, pucName = NULL)
```

## Arguments

- archive:

  path to a tgz-file as created by [`retrieveData`](retrieveData.md)

- pucName:

  (Optional) name (without the `.puc` extension) the puc-file should be
  given. Only needed for archives created before `retrieveData` started
  recording this name in `config.rds`.

## Value

Invisibly, the path to the created puc-file.

## Note

This only works as long as the madrat cache files that were used to
compute the archive are still available on this machine, as the archive
itself only contains the aggregated collection, not the underlying cache
files. If some of these cache files are missing `pucCreate` will fail
with an error naming them.

## See also

[`retrieveData`](retrieveData.md),[`pucAggregate`](pucAggregate.md)

Other aggregation: [`pucAggregate()`](pucAggregate.md),
[`toolAggregate()`](toolAggregate.md)

## Author

Patrick Rein

## Examples

``` r
if (FALSE) { # \dontrun{
pucCreate("rev1_h12_example.tgz")
} # }
```
