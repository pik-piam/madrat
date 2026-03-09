# pucAggregate

Function which takes a puc-file ("portable unaggregated collection") as
created via [`retrieveData`](retrieveData.md) and computes the
corresponding aggregated collection with the provided arguments (e.g.
the provided region mapping). The resulting tgz-file containing the
collection will be put to the madrat outputfolder as defined in
`getConfig("outputfolder")`.

## Usage

``` r
pucAggregate(
  puc,
  regionmapping = getConfig("regionmapping"),
  ...,
  renv = TRUE,
  strict = FALSE
)
```

## Arguments

- puc:

  path to a puc-file

- regionmapping:

  region mapping to be used for aggregation.

- ...:

  (Optional) Settings that should be changed in addition. NOTE: which
  settings can be modified varies from puc to puc. Allowed settings are
  typically listed in the file name of the puc file after the revision
  number.

- renv:

  Boolean which determines whether data should be aggregated from within
  a renv environment (recommended) or not. If activated, `renv` will
  check which packages in which versions were used to create the puc
  file, download, install and load these packages and run the
  aggregation with them. Otherwise, the packages in the currently used
  environment are being used.

- strict:

  Boolean or NULL which allows to trigger a strict mode. During strict
  mode warnings will be taken more seriously and will cause 1. to have
  the number of warnings as prefix of the created tgz file and 2. will
  prevent `retrieveData` from creating a puc file. If set to NULL the
  setting will be read from the puc file.

## See also

[`retrieveData`](retrieveData.md),[`localConfig`](setConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
pucAggregate("rev1_example.puc", regionmapping = "regionmappingH12.csv")
} # }
```
