# Tool: Start message

This function writes a process start message (what function was called
with which arguments) and stores the current time, so the corresponding
call to [`toolendmessage`](toolendmessage.md) can calculate the elapsed
time.

## Usage

``` r
toolstartmessage(functionCallString, level = NULL)
```

## Arguments

- functionCallString:

  A string representing the function call that should be logged

- level:

  This argument allows to establish a hierarchy of print statements. The
  hierarchy is preserved for the next vcat executions. Currently this
  setting can have 4 states: NULL (nothing will be changed), 0 (reset
  hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
  hierarchy level by 1).

## Value

A list containing diagnostic information required by
[`toolendmessage`](toolendmessage.md).

## See also

[`toolendmessage`](toolendmessage.md), [`vcat`](vcat.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
innerFunction <- function() {
  startinfo <- madrat:::toolstartmessage("innerFunction(argumentsToPrint = 123)", "+")
  vcat(1, "inner")
  madrat:::toolendmessage(startinfo, "-")
}
outerFunction <- function() {
  startinfo <- madrat:::toolstartmessage("outerFunction()", "+")
  vcat(1, "outer")
  innerFunction()
  madrat:::toolendmessage(startinfo, "-")
}
outerFunction()
#> Run outerFunction()
#> ~ NOTE: outer
#> ~ Run innerFunction(argumentsToPrint = 123)
#> ~~ NOTE: inner
#> ~ Exit innerFunction(argumentsToPrint = 123) in 0 seconds
#> Exit outerFunction() in 0 seconds
```
