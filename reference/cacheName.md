# cacheName

Get the name of a cache file corresponding to the given args

## Usage

``` r
cacheName(prefix, type, args = NULL)
```

## Arguments

- prefix:

  function prefix (e.g. "calc" or "read")

- type:

  output type (e.g. "TauTotal")

- args:

  a list of named arguments used to call the given function

## Value

absolute path to a cache file that does not necessarily exist

## Note

With `setConfig(forcecache=TRUE)` cacheName will also return cache file
names with deviating fingerprint if no fitting cache file is found (if
there are multiple it will just return the newest one).

## See also

[`cachePut`](cachePut.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
madrat:::cacheName("calc", "TauTotal")
#> [1] "/tmp/RtmpKpbvAy/madrat/cache/default/calcTauTotal-F8cdfa51c.rds"
```
