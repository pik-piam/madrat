# visualizeDependencies

Creates a graphical visualization of dependencies between functions in
the mr-universe.

## Usage

``` r
visualizeDependencies(
  ...,
  direction = "both",
  order = 2,
  filter = NULL,
  packages = getConfig("packages"),
  filename = NULL
)
```

## Arguments

- ...:

  function(s) to be analyzed

- direction:

  Character string, either “in”, “out” or "both". If “in” all sources
  feeding into the function are listed. If “out” consumer of the
  function are listed. If “both” the union of "in" and "out" is
  returned.

- order:

  order of dependencies. Order 1 would be only functions directly called
  from (in case of direction "in") or directly calling (in case of
  direction "out") are shown. Order 2 will also show direct dependencies
  of the order 1 dependencies, order 3 also the direct dependencies from
  order 2 dependencies, etc.

- filter:

  regular expression to describe elements which should be excluded from
  visualization (e.g. "^tool" to exclude all tool functions)

- packages:

  packages to use when searching dependencies

- filename:

  If a filename is provided, the resulting graph will be saved

## See also

[`getDependencies`](getDependencies.md),
[`getMadratGraph`](getMadratGraph.md),
[`getMadratInfo`](getMadratInfo.md)

## Author

Debbora Leip, Jan Philipp Dietrich
