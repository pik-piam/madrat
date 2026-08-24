# cacheFormat

Look up a registered cache format definition.

## Usage

``` r
cacheFormat(name = getConfig("cacheformat"))
```

## Arguments

- name:

  Name of the format, defaults to the currently configured one.

## Value

The format definition, with the format name added as element "name".
