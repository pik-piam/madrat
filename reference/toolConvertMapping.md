# Tool: ConvertMapping

Function which converts mapping files between formats

## Usage

``` r
toolConvertMapping(name, format = "rds", type = NULL, where = "mappingfolder")
```

## Arguments

- name:

  File name of the mapping file. Supported file types are currently csv
  (, or ; separated), rds and rda (which needs to have the data stored
  with the object name "data"!).

- format:

  format it should be converted to. Available is "csv", "rds" or "rda".

- type:

  Mapping type (e.g. "regional", "cell", or "sectoral"). Can be set to
  NULL if file is not stored in a type specific subfolder

- where:

  location to look for the mapping, either "mappingfolder" or the name
  of a package which contains the mapping

## See also

[`calcOutput`](calcOutput.md), `toolConvertMapping`

## Author

Jan Philipp Dietrich
