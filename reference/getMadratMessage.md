# getMadratMessage

Read a madrat message from the madrat environment. The madrat
environment behaves similar like global options, except that 1) messages
will also be stored in cache files and restored when a cache file is
being loaded and 2) messages are always stored in lists with messages
split by function calls where the message was triggered.

## Usage

``` r
getMadratMessage(name = NULL, fname = NULL)
```

## Arguments

- name:

  The category in which the message should be stored

- fname:

  function name. If specified only messages belonging to the functions
  history will be returned (this includes entries from the function
  itself, but also entries from functions which were called by this
  function).

## See also

`getMadratMessage`

## Author

Jan Philipp Dietrich

## Examples

``` r
putMadratMessage("test", "This is a toast", fname = "readTau")
getMadratMessage("test", fname = "calcTauTotal")
#> $readTau
#> [1] "This is a toast"
#> 
```
