# getLinkFunction

Returns a function that creates a symlink, hardlink, junction, or copy
of files and directories, depending on OS capabilities (usually symlinks
are not supported on Windows).

## Usage

``` r
getLinkFunction()
```

## Value

A function with arguments "from" and "to" which should behave like
file.symlink on all platforms.

## Author

Pascal Sauer
