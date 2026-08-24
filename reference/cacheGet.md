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
cache file name that should be written if the data has to be
recalculated. attr(, "readFile") will be set to the cache file that was
successfully read, and is absent otherwise. This can differ from attr(,
"id") if the data was read from an rds file while another cache format
is configured.

## See also

[`cachePut`](cachePut.md), [`cacheNames`](cacheNames.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
madrat:::cacheGet("calc", "TauTotal")
#> [1] NA
#> attr(,"id")
#> [1] "/tmp/RtmpCrlm26/madrat/cache/default/calcTauTotal-F201f3fe1.rds"
```
