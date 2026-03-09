# Tool: GetMapping

Function which retrieves a mapping file

## Usage

``` r
toolGetMapping(
  name,
  type = NULL,
  where = NULL,
  error.missing = TRUE,
  returnPathOnly = FALSE,
  activecalc = NULL
)
```

## Arguments

- name:

  File name of the mapping file. Supported file types are currently csv
  (, or ; separated), rds and rda (which needs to have the data stored
  with the object name "data"!). Use
  [`toolConvertMapping`](toolConvertMapping.md) to convert between both
  formats

- type:

  Mapping type (e.g. "regional", "cell", or "sectoral"). Can be set to
  NULL if file is not stored in a type specific subfolder

- where:

  location to look for the mapping, either "mappingfolder", "local" (if
  the path is relative to your current directory) or the name of a
  package which contains the mapping. If set to NULL it will first try
  "local", then "mappingfolder" and afterwards scan all packages
  currently listed in `getConfig("packages")`

- error.missing:

  Boolean which decides whether an error is returned if the mapping file
  does not exist or not.

- returnPathOnly:

  If set to TRUE only the file path is returned

- activecalc:

  If set, this argument helps to define the first package within which
  the mapping has to be sought for. This happens via finding in which
  package the active calc function is located.

## Value

the mapping as a data frame

## See also

[`calcOutput`](calcOutput.md),
[`toolConvertMapping`](toolConvertMapping.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
head(toolGetMapping("regionmappingH12.csv", where = "madrat"))
#>               X CountryCode RegionCode
#> 1         Aruba         ABW        LAM
#> 2   Afghanistan         AFG        OAS
#> 3        Angola         AGO        SSA
#> 4      Anguilla         AIA        LAM
#> 5 Aland Islands         ALA        EUR
#> 6       Albania         ALB        NEU
```
