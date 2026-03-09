# madratAttach / madratDetach

Attaches the madrat functions of a package to the currently active
madrat universe or detaches it again from it.

## Usage

``` r
madratAttach(package)

madratDetach(package)
```

## Arguments

- package:

  name of the package to be loaded. Alternative, the path to the
  package.

## Functions

- `madratDetach()`: detach package from madrat universe

## See also

[`getConfig`](getConfig.md), [`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
madratAttach("madrat")
} # }
```
