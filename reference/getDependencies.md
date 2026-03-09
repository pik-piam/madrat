# getDependencies

Returns information about dependencies of a madrat-based calc- read- or
full-function.

## Usage

``` r
getDependencies(
  name,
  direction = "in",
  graph = NULL,
  type = NULL,
  self = FALSE,
  ...
)
```

## Arguments

- name:

  name of the function to be analyzed

- direction:

  Character string, either “in”, “out”, "both", “full”, "din" or "dout".
  If “in” all sources feeding into the function are listed. If “out”
  consumer of the function are listed. If “both” the union of "in" and
  "out" is returned. If "full" the full network this function is
  connected to is shown, including indirect connections to functions
  which neither source nor consume the given function but serve as
  sources to other consumer functions. "din" and "dout" (short for
  "direct in" and "direct out") behave like "in" and "out" but only show
  direct calls in or from the function (ignoring the network of
  functions attached to it).

- graph:

  A madrat graph as returned by [`getMadratGraph`](getMadratGraph.md).
  Will be created with [`getMadratGraph`](getMadratGraph.md) if not
  provided.

- type:

  type filter. Only dependencies of that type will be returned.
  Currently available types are "calc", "read" and "tool"

- self:

  boolean defining whether the function itself, which is analyzed,
  should be included in the output, or not

- ...:

  Additional arguments for [`getMadratGraph`](getMadratGraph.md) in case
  that no graph is provided (otherwise ignored)

## See also

[`getCalculations`](getCalculations.md),
[`getMadratGraph`](getMadratGraph.md),
[`getMadratInfo`](getMadratInfo.md)

## Author

Jan Philipp Dietrich
