# addMapping

Function whichs adds another mapping to the current list of
extramappings in the madrat configuration (see
[`setConfig`](setConfig.md)) and stores the mapping in the mapping
folder as well as output folder.

## Usage

``` r
addMapping(filename, mapping = NULL)
```

## Arguments

- filename:

  The name of the the region mapping that should added including file
  ending (e.g. "regionmappingREMIND.csv"). Supported formats are
  currently ".csv" and ".rds".

- mapping:

  Mapping provided as data.frame, or NULL. If a mapping is provided the
  data will be written in the mapping file of the given file
  (potentially replacing existing data). If NULL the mapping from the
  given file is used.

## See also

[`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
addMapping("regionmappingH12.csv")
} # }
```
