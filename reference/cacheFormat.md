# cacheFormat

Look up a registered cache format definition.

## Usage

``` r
cacheFormat(name = getConfig("cacheformat"))

cacheFormatProblem(name)
```

## Arguments

- name:

  Name of the format, defaults to the currently configured one.

## Value

The format definition, with the format name added as element "name".

## Functions

- `cacheFormatProblem()`: explain why a format cannot be used (required
  packages missing), or return NULL if it can. Must be checked whenever
  a cache format is selected (setConfig, initializeConfig). Checking it
  in `cacheFormat` is not sufficient, as the error would then be masked
  by the surrounding cache I/O error handling.
