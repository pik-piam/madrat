# cacheRead / cacheWrite / cacheToRds

Read, write and convert cache files using the format belonging to their
file extension. Dispatching on the extension rather than on the
configured format is what allows madrat to still read rds cache files
while writing a different format (see [`cacheNames`](cacheNames.md)).

## Usage

``` r
cacheRead(file)

cacheWrite(x, file)

cacheToRds(input, output)
```

## Arguments

- file:

  Path of the cache file to be read/written, including file extension.

- x:

  Object to be written.

- input:

  Path of the cache file to be converted.

- output:

  Path of the rds file to be created.

## Functions

- `cacheWrite()`: write a cache file

- `cacheToRds()`: convert a cache file to rds

## Author

Patrick Rein
