# initializeConfig

Checks whether configuration already has been set. If not, it will be
initialized with default settings or (if available) system settings. All
madrat folders (see [`setConfig`](setConfig.md) for documentation which
folders are available) will be set to the system environment variables
MADRAT_SOURCEFOLDER, MADRAT_CACHEFOLDER, etc. if they exist, NA
otherwise. NA means subfolders of the mainfolder are used.

## Usage

``` r
initializeConfig(verbose = TRUE)
```

## Arguments

- verbose:

  boolean deciding whether status information/updates should be shown or
  not

## See also

[`getMainfolder`](getMainfolder.md), [`getConfig`](getConfig.md),
[`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich
