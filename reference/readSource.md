# readSource

Read in a source file and convert it to a MAgPIE object. The function is
a wrapper for specific functions designed for the different possible
source types.

## Usage

``` r
readSource(
  type,
  subtype = NULL,
  subset = NULL,
  convert = TRUE,
  supplementary = FALSE
)
```

## Arguments

- type:

  A character string referring to the source type, e.g. "IEA" which
  would internally call a function called \`readIEA\` (the "wrapped
  function"). A list of available source types can be retrieved with
  function [`getSources`](getSources.md).

- subtype:

  A character string. For some sources there are subtypes of the source,
  for these sources the subtype can be specified with this argument. If
  a source does not have subtypes, subtypes should not be set.

- subset:

  A character string. Similar to `subtype` a source can also have
  `subsets`. A `subsets` can be used to only read part of the data. This
  can in particular make sense for huge data sets where reading in the
  whole data set might be impractical or even infeasible.

- convert:

  Boolean indicating whether input data conversion to ISO countries
  should be done or not. In addition it can be set to "onlycorrect" for
  sources with a separate correctXXX-function.

- supplementary:

  Boolean deciding whether a list including the actual data and
  metadata, or just the actual data is returned.

## Value

The read-in data, usually a magpie object. If supplementary is TRUE a
list including the data and metadata is returned instead. The temporal
and data dimensionality should match the source data. The spatial
dimension should either match the source data or, if the convert
argument is set to TRUE, should be on ISO code country level.

## Note

If a magpie object is returned magclass::clean_magpie is run and if
convert = TRUE ISO code country level is checked.

The underlying read-functions can return a magpie object or a list of
information (preferred) back to `readSource`. In list format the object
should have the following structure:

- **x** - the data itself as magclass object

- **unit** (optional) - unit of the provided data

- **description** (otional) - a short description of the data

- **note** (optional) - additional notes related to the data

- **class** (optional \| default = "magpie") - Class of the returned
  object. If set to something other than "magpie" most functionality
  will not be available and is switched off!

- **cache** (optional) boolean which decides whether a cache file should
  be written (if caching is active) or not. Default setting is TRUE.
  This can be for instance useful, if the calculation itself is quick,
  but the corresponding file sizes are huge. Or if the caching for the
  given data type does not support storage in RDS format. CAUTION:
  Deactivating caching for a data set which should be part of a PUC file
  will corrupt the PUC file. Use with care.

## See also

[`setConfig`](setConfig.md), [`downloadSource`](downloadSource.md),
[`readTau`](readTau.md)

## Author

Jan Philipp Dietrich, Anastasis Giannousakis, Lavinia Baumstark, Pascal
Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("Tau", "paper")
} # }
```
