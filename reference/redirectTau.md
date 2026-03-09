# redirectTau

redirectTau will be called by redirectSource when type = "Tau".
Redirects the Tau source folder to the target folder.

## Usage

``` r
redirectTau(target, ...)
```

## Arguments

- target:

  The target folder or files.

- ...:

  Passed on to [`redirect`](redirect.md).

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
redirectSource("Tau", "a/different/tau-source-folder")
a <- readSource("Tau", "paper")
} # }
```
