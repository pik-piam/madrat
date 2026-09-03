# startMemoryProfiling

Starts a memory profiling block by resetting the kernel's peak resident
set size counter, so that the peak reported by
[`reportMemoryProfiling`](reportMemoryProfiling.md) refers to this block
only. As that counter is process wide, profiling blocks must not
overlap.

## Usage

``` r
startMemoryProfiling()
```

## Value

The resident set size at the start of the block in kB, or NULL if
profiling is unavailable on this system

## See also

[`reportMemoryProfiling`](reportMemoryProfiling.md)

## Author

Patrick Rein
