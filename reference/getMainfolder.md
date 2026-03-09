# getMainfolder

Functions checks for a global setting of the mainfolder (either by
setting the environment variable "MADRAT_MAINFOLDER" or by setting the R
option with the same name). If none of these is available the user will
be asked for a directory. If this is not provided a temporary folder
will be used.

## Usage

``` r
getMainfolder(verbose = TRUE, .testmode = FALSE)
```

## Arguments

- verbose:

  boolean deciding whether status information/updates should be shown or
  not

- .testmode:

  boolean switch only relevant for internal testing (will simulate user
  inputs)

## See also

[`initializeConfig`](initializeConfig.md), [`getConfig`](getConfig.md),
[`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich
