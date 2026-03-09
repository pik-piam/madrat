# cacheCopy

Copy cache files which were used for a given preprocessing.

## Usage

``` r
cacheCopy(file, target = NULL, filter = NULL)
```

## Arguments

- file:

  Path to a log file or content of a log as character vector.

- target:

  Folder to which the files should be copied. If NULL no data is copied.

- filter:

  Regular expression to filter the cache files shown in the log file.

## Value

A vector of cache files which match the given log information and
filter.

## Author

Jan Philipp Dietrich
