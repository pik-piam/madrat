# getLocation

Returns names of packages in which functions matching the description
are found

## Usage

``` r
getLocation(name, packages = installedMadratUniverse(), globalenv = TRUE)
```

## Arguments

- name:

  name of the function to be found. Can be either the full name (e.g.
  "calcTauTotal"), or just the type name (e.g. "TauTotal").

- packages:

  A character vector with packages in which should be looked for the
  function

- globalenv:

  Boolean deciding whether functions in the global environment should be
  included or not

## Value

vector of packages in which a function matching the description could be
found

## See also

[`getCalculations`](getCalculations.md),
[`getDependencies`](getDependencies.md)

## Author

Jan Philipp Dietrich
