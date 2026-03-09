# getSourceFolder

Return the path to source data files for the given type and subtype.
This applies redirections, see [`redirectSource`](redirectSource.md) for
more details.

## Usage

``` r
getSourceFolder(type, subtype)
```

## Arguments

- type:

  Dataset name, e.g. "Tau" for [`readTau`](readTau.md)

- subtype:

  Subtype of the dataset, e.g. "paper" for [`readTau`](readTau.md), NULL
  is allowed

## Value

Path to source data files

## Author

Pascal Sauer
