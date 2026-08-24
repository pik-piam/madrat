# cachePut

Save data to cache

## Usage

``` r
cachePut(x, prefix, type, fname, callString)
```

## Arguments

- x:

  data that should be written to cache

- prefix:

  function prefix (e.g. "calc" or "read")

- type:

  output type (e.g. "TauTotal")

- fname:

  The name of the cache file to be written.

- callString:

  A string representation of the function call that leads to the cache
  file being written. Will be attached as an attribute.

## Value

`fname` if the cache file was written, otherwise NULL. Writing a cache
file is optional and allowed to fail, so callers which need to know
whether the file exists (e.g. to list it in a puc file) must use this
return value rather than assume `fname` was created.

## See also

`cachePut`, [`cacheNames`](cacheNames.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
fname <- madrat:::cacheNames("calc", "Example")$write
madrat:::cachePut(1, "calc", "Example", fname, 'calcOutput("Example")')
} # }
```
