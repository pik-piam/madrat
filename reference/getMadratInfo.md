# getMadratInfo

Collects and returns detailed information about the currently loaded
network of madrat functions.

## Usage

``` r
getMadratInfo(graph = NULL, cutoff = 5, extended = FALSE, ...)
```

## Arguments

- graph:

  A madrat graph as returned by [`getMadratGraph`](getMadratGraph.md).
  Will be created with [`getMadratGraph`](getMadratGraph.md) if not
  provided.

- cutoff:

  Integer introducing a cutoff of items to be returned for outputs which
  can become quite verbose.

- extended:

  Will add additional outputs which has been removed from standard
  output due to limited usefulness.

- ...:

  Additional arguments for [`getMadratGraph`](getMadratGraph.md) in case
  that no graph is provided (otherwise ignored)

## See also

[`getCalculations`](getCalculations.md),
[`getMadratGraph`](getMadratGraph.md)

## Author

Jan Philipp Dietrich
