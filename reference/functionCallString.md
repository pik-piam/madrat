# functionCallString

Create a string representation for a function call. If the resulting
string is longer than getConfig("maxLengthLogMessage") arguments are
printed as passed (e.g. as variable name instead of the evaluated
content of that variable), if that is still too long it is cropped.

## Usage

``` r
functionCallString(functionName, argumentValues)
```

## Arguments

- functionName:

  name of the called function

- argumentValues:

  the list of arguments passed

## Value

A string representing the given function call

## Author

Pascal Sauer
