# memoryProfilingSupported

Checks whether the kernel interfaces required for memory profiling are
available. Only Linux exposes both the high-water mark of the resident
set size and a way to reset it.

## Usage

``` r
memoryProfilingSupported()
```

## Value

TRUE if memory profiling can be performed, FALSE otherwise

## See also

[`setConfig`](setConfig.md)

## Author

Patrick Rein
