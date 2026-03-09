# Tool: Verbosity Cat

Function which returns information based on the verbosity setting

## Usage

``` r
vcat(
  verbosity,
  ...,
  level = NULL,
  fill = TRUE,
  show_prefix = TRUE,
  logOnly = FALSE
)
```

## Arguments

- verbosity:

  The lowest verbosity level for which this message should be shown
  (verbosity = -1 means no information at all, 0 = only warnings, 1 =
  warnings and execution information, 2 = full information). If the
  verbosity is set to 0 the message is written as warning, if the
  verbosity is set higher than 0 it is written as a normal cat message.

- ...:

  The message to be shown

- level:

  This argument allows to establish a hierarchy of print statements. The
  hierarchy is preserved for the next vcat executions. Currently this
  setting can have 4 states: NULL (nothing will be changed), 0 (reset
  hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
  hierarchy level by 1).

- fill:

  a logical or (positive) numeric controlling how the output is broken
  into successive lines. If FALSE (default), only newlines created
  explicitly by "\n" are printed. Otherwise, the output is broken into
  lines with print width equal to the option width if fill is TRUE, or
  the value of fill if this is numeric. Non-positive fill values are
  ignored, with a warning.

- show_prefix:

  a logical defining whether a content specific prefix (e.g. "NOTE")
  should be shown in front of the message or not. If prefix is not shown
  it will also not show up in official statistics.

- logOnly:

  option to only log warnings and error message without creating
  warnings or errors (expert use only).

## See also

[`readSource`](readSource.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
vcat(2, "Hello world!")
} # }
```
