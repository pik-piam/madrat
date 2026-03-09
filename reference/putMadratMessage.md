# putMadratMessage

Store a madrat message in the madrat environment. The madrat environment
behaves similar like global options, except that 1) messages will also
be stored in cache files and restored when a cache file is being loaded
and 2) messages are always stored in lists with messages split by
function calls where the message was triggered.

## Usage

``` r
putMadratMessage(name, value, fname = -1, add = FALSE)
```

## Arguments

- name:

  The category in which the message should be stored

- value:

  The message that should be recorded as character. Alternatively, if
  `name` is not set, it is also possible to provide a complete list of
  the structure value\[\[name\]\]\[\[fname\]\] where name and fname
  correspond to the category name and function name entries (e.g.
  `value = list(test = list(readTau = "This is a toast"))`).

- fname:

  function name the entry belongs to or the frame number from which the
  function name should be derived from (e.g. -1 to recieve function name
  from parent function).

- add:

  boolean deciding whether the value should be added to a existing value
  (TRUE) or overwrite it (FALSE)

## See also

`putMadratMessage`

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
