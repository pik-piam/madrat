# Tool: withMadratLogging

Function will activate madrat logging facilities for all code provided
to this function. This means that `message`, `warning` and `stop` calls
will also report to the madrat log output

## Usage

``` r
withMadratLogging(expr, logOnly = TRUE, warningsAsErrors = FALSE)
```

## Arguments

- expr:

  expression to be evaluated.

- logOnly:

  passed to vcat, determines if warning/error is thrown after logging.
  If omitted in a nested call, the enclosing logging policy is
  inherited.

- warningsAsErrors:

  whether warnings should be logged and handled as errors. If omitted in
  a nested call, the enclosing logging policy is inherited.

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
