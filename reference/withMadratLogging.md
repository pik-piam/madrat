# Tool: withMadratLogging

Function will activate madrat logging facilities for all code provided
to this function. This means that `message`, `warning` and `stop` calls
will also report to the madrat log output

## Usage

``` r
withMadratLogging(expr, logOnly = TRUE)
```

## Arguments

- expr:

  expression to be evaluated.

- logOnly:

  passed to vcat, determines if warning/error is thrown after logging

## See also

[`vcat`](vcat.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
madrat:::withMadratLogging(message("Hello world!"))
} # }
```
