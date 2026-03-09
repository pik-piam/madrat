# getFlags

Support function which extracts flags from code. Flags are string
literals in a function body, for example \`"!# @pucArguments extra"\`.

## Usage

``` r
getFlags(code)
```

## Arguments

- code:

  A character vector with code from functions to be analyzed

## Value

A list of found flag entries

## See also

[`getCode`](getCode.md)

## Author

Jan Philipp Dietrich
