# prepExtendedComment

Helper function condense metadata information into an extended comment
entry

## Usage

``` r
prepExtendedComment(x, type, functionCallString, warn = TRUE)
```

## Arguments

- x:

  list containing the metadata to be condensed

- type:

  output type, e.g. "TauTotal"

- functionCallString:

  A string representation of the function call that created the object
  this comment is attached to

- warn:

  boolean indicating whether warnings should be triggered if entries are
  missing, or not.

## Author

Jan Philipp Dietrich, Pascal Sauer
