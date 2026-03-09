# Tool: ManualDownload

Support tool for the creation of download functions in cases where a
fully automated data download is not an option (e.g. due to a missing
API). The function can be used to print a step-by-step guide for the
user how to manually retrieve the data and then asks for a (local) path
where the data can be copied from.

## Usage

``` r
toolManualDownload(
  instructions,
  intro = "Data must be downloaded manually",
  request = "Enter full path to the downloaded data:"
)
```

## Arguments

- instructions:

  Download instructions in form of a character vector describing how to
  manually retrieve the data.

- intro:

  Introductory sentence to be shown first. Will not show up if set to
  NULL.

- request:

  A prompt which should show up after the instructions to ask for the
  local download location.

## See also

[`downloadSource`](downloadSource.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
toolManualDownload(c("Log into website ABC",
                     "Download the data set XYZ"))
} # }
```
