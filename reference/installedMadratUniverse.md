# installedMadratUniverse

Returns a name vector of installed packages which supposedly belong to
the madrat universe. They are currently derived as the union of

- all packages registered under `getConfig("packages")`,

- all packages with a name starting with "mr" or "ms" (as the usual
  indicator for madrat-packages and madrat-support-packages), and

- all packages having `madrat` as either a `Depends` or `Imports`
  dependency.

## Usage

``` r
installedMadratUniverse()
```

## Value

A name vector of installed packages which supposedly belong to the
madrat universe

## See also

[`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
installedMadratUniverse()
} # }
```
