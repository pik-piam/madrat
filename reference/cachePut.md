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

## See also

`cachePut`, [`cacheName`](cacheName.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
fname <- madrat:::cacheName("calc", "Example")
madrat:::cachePut(1, "calc", "Example", fname, 'calcOutput("Example")')
} # }
```
