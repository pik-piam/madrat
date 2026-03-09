# resetMadratMessages

Delete stored madrat messages from the madrat environment. The madrat
environment behaves similar like global options, except that 1) messages
will also be stored in cache files and restored when a cache file is
being loaded and 2) messages are always stored in lists with messages
split by function calls where the message was triggered.

## Usage

``` r
resetMadratMessages(name = NULL, fname = NULL)
```

## Arguments

- name:

  The category for which the messages should be reset (if not set
  messages in all categories will be reset)

- fname:

  function name for which the entries should be reset (if not specified
  messages for all function names will be reset)

## See also

[`putMadratMessage`](putMadratMessage.md),
[`getMadratMessage`](getMadratMessage.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
putMadratMessage("test", "This is a toast", fname = "readTau")
getMadratMessage("test", fname = "calcTauTotal")
#> $readTau
#> [1] "This is a toast"
#> 
resetMadratMessages("test")
```
