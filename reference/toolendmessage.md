# Tool: End message

This function writes a process end message and performs some
diagnostics. It is always called after a corresponding call to
[`toolstartmessage`](toolstartmessage.md).

## Usage

``` r
toolendmessage(startdata, level = NULL)
```

## Arguments

- startdata:

  a list containing diagnostic information provided by
  [`toolstartmessage`](toolstartmessage.md)

- level:

  This argument allows to establish a hierarchy of print statements. The
  hierarchy is preserved for the next vcat executions. Currently this
  setting can have 4 states: NULL (nothing will be changed), 0 (reset
  hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
  hierarchy level by 1).

## See also

[`toolstartmessage`](toolstartmessage.md), [`vcat`](vcat.md)

## Author

Jan Philipp Dietrich
