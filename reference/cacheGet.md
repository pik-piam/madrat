# Tool: cacheGet

Load fitting cache data (if available)

## Usage

``` r
cacheGet(prefix, type, args = NULL)
```

## Arguments

- prefix:

  function prefix (e.g. "calc" or "read")

- type:

  output type (e.g. "TauTotal")

- args:

  a list of named arguments used to call the given function

## Value

cached data if available, otherwise NA attr(, "id") will be set to the
cache file name that was (tried to be) loaded

## See also

[`cachePut`](cachePut.md), [`cacheName`](cacheName.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
madrat:::cacheGet("calc", "TauTotal")
#> [1] NA
#> attr(,"id")
#> [1] "/tmp/RtmpW4xoz8/madrat/cache/default/calcTauTotal-F8cdfa51c.rds"
```
