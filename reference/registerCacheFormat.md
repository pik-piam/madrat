# registerCacheFormat

Register a serialization format which can be used for madrat cache files
via `setConfig(cacheformat = ...)`. The formats "rds" (the default) and
"qs2" are always available, "qs2" requires the `qs2` package.

## Usage

``` r
registerCacheFormat(name, write, read, extension = name, toRds = NULL)

cacheFormats()
```

## Arguments

- name:

  Name of the format, e.g. "qs2".

- write:

  A function(x, file) writing object `x` to `file`.

- read:

  A function(file) returning the object stored in `file`.

- extension:

  File extension used for cache files of this format (without leading
  dot). Defaults to `name`.

- toRds:

  Optional fast path conversion function(input, output) converting a
  cache file of this format to a rds file. This is used when bundling
  puc files, which always contain rds files.

## Value

Invisibly, the registered format definition.

## Details

Cache files are identified by their file extension, so each format must
use a distinct one. Extensions are restricted to alphanumeric
characters.

## Functions

- `cacheFormats()`: names of all currently registered cache formats

## See also

[`setConfig`](setConfig.md), `cacheFormats`

Other cache management: [`cacheCleanup()`](cacheCleanup.md),
[`cacheCopy()`](cacheCopy.md)

## Author

Patrick Rein

## Examples

``` r
if (FALSE) { # \dontrun{
registerCacheFormat("qs", write = qs::qsave, read = qs::qread)
setConfig(cacheformat = "qs")
} # }
```
