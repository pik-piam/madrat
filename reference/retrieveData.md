# retrieveData

Function to retrieve a predefined collection of calculations for a
specific regionmapping.

## Usage

``` r
retrieveData(
  model,
  rev = 0,
  dev = "",
  cachetype = "def",
  puc = identical(dev, ""),
  strict = FALSE,
  renv = TRUE,
  ...
)
```

## Arguments

- model:

  The names of the model for which the data should be provided (e.g.
  "magpie").

- rev:

  data revision which should be used/produced. Will be converted to
  [`numeric_version`](https://rdrr.io/r/base/numeric_version.html).

- dev:

  development suffix to distinguish development versions for the same
  data revision. This can be useful to distinguish parallel lines of
  development.

- cachetype:

  defines what cache should be used. "rev" points to a cache shared by
  all calculations for the given revision and sets forcecache to TRUE,
  "def" points to the cache as defined in the current settings and does
  not change forcecache setting.

- puc:

  Boolean deciding whether a fitting puc file (if existing) should be
  read in and if a puc file (if not already existing) should be created.

- strict:

  Boolean which allows to trigger a strict mode. During strict mode
  warnings will be taken more seriously and will cause 1. to have the
  number of warnings as prefix of the created tgz file and 2. will
  prevent `retrieveData` from creating a puc file.

- renv:

  Boolean which determines whether calculations should run within a renv
  environment (recommended) or not (currently only applied in
  [`pucAggregate`](pucAggregate.md)). If activated, `renv` will check
  which packages in which versions were used to create the puc file,
  download, install and load these packages and run the aggregation with
  them. Otherwise, the packages in the currently used environment are
  being used.

- ...:

  (Optional) Settings that should be changed using `setConfig` (e.g.
  regionmapping). or arguments which should be forwarded to the
  corresponding fullXYZ function (Please make sure that argument names
  in full functions do not match settings in `setConfig`!)

## Value

Invisibly, the path to the newly created tgz archive.

## Note

The underlying full-functions can optionally provide a list of
information back to `retrieveData`. Following list entries are currently
supported:

- **tag** (optional) - additional name tag which will be included in the
  file name of the aggregated collection (resulting tgz-file). This can
  be useful to highlight information in the file name which otherwise
  would not be visible.

- **pucTag** (optional) - identical purpose as **tag** but for the
  resulting unaggregated collections (puc-files).

## See also

[`calcOutput`](calcOutput.md),[`setConfig`](setConfig.md)

## Author

Jan Philipp Dietrich, Lavinia Baumstark

## Examples

``` r
if (FALSE) { # \dontrun{
retrieveData("example", rev = "2.1.1", dev = "test", regionmapping = "regionmappingH12.csv")
} # }
```
