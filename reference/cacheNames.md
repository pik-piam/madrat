# cacheNames

Get the names of the cache files corresponding to the given args

## Usage

``` r
cacheNames(prefix, type, args = NULL)
```

## Arguments

- prefix:

  function prefix (e.g. "calc" or "read")

- type:

  output type (e.g. "TauTotal")

- args:

  a list of named arguments used to call the given function

## Value

a list with two elements: `write`, the absolute path to the cache file
that should be written for the given arguments (in the configured cache
format), which does not necessarily exist, and `read`, the absolute path
of an already existing, fitting cache file which should be read
(possibly in another format), or NULL if there is none.

## Note

With `setConfig(forcecache=TRUE)` cacheNames will also look for cache
files with deviating fingerprint if no fitting cache file is found (if
there are multiple it will just use the newest one).

Cache files are searched for in the configured cache format (see
[`setConfig`](setConfig.md)) first and in "rds" second. This way an
existing rds cache stays usable after switching to another cache format.
Files found via this fallback are only read, never rewritten, so the
returned write target always uses the configured format.

## See also

[`cachePut`](cachePut.md)

## Author

Patrick Rein, Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
madrat:::cacheNames("calc", "TauTotal")
#> $write
#> [1] "/tmp/RtmpAE9h8d/madrat/cache/default/calcTauTotal-F201f3fe1.rds"
#> 
#> $read
#> NULL
#> 
```
