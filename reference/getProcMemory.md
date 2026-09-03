# getProcMemory

Reads the current and the peak resident set size of this process as
recorded by the kernel.

## Usage

``` r
getProcMemory()
```

## Value

A named numeric vector with the entries "rss" (VmRSS) and "peak"
(VmHWM), both in kB

## Author

Patrick Rein
