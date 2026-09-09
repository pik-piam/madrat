# cacheFormat

Look up a registered cache format definition.

## Usage

``` r
cacheFormat(name = getConfig("cacheformat"))

checkCacheFormatAvailable(name, hint = NULL)
```

## Arguments

- name:

  Name of the format, defaults to the currently configured one.

- hint:

  Optional text appended to the error message, e.g. to point at the
  environment variable which caused an unusable format to be selected.

## Value

The format definition, with the format name added as element "name".

## Functions

- `checkCacheFormatAvailable()`: check that a format's required packages
  (see [`registerCacheFormat`](registerCacheFormat.md)) are installed.
  Must be called whenever a cache format is selected (setConfig,
  initializeConfig). Calling it in `cacheFormat` is not sufficient, as
  the error would then be masked by the surrounding cache I/O error
  handling.
