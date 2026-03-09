# isWrapperActive

Support functions which checks whether a given wrapper function is
currently in-use or not or which locally activate or deactivate a
wrapper (setting will be automatically resetted when a function
finishes).

## Usage

``` r
isWrapperActive(name)

setWrapperActive(name)

setWrapperInactive(name)
```

## Arguments

- name:

  name of the wrapper in question (e.g. "calcOutput")

## Functions

- `setWrapperActive()`: set wrapper activity status to on

- `setWrapperInactive()`: set wrapper activity status to off

## Author

Jan Philipp Dietrich
