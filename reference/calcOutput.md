# calcOutput

Calculate a specific output for which a calculation function exists. The
function is a wrapper for specific functions designed for the different
possible output types.

## Usage

``` r
calcOutput(
  type,
  aggregate = TRUE,
  file = NULL,
  years = NULL,
  round = NULL,
  signif = NULL,
  supplementary = FALSE,
  append = FALSE,
  warnNA = TRUE,
  na_warning = NULL,
  try = FALSE,
  regionmapping = NULL,
  writeArgs = NULL,
  temporalmapping = NULL,
  ...,
  outputStatistics = NULL
)
```

## Arguments

- type:

  output type, e.g. "TauTotal". A list of all available source types can
  be retrieved with function [`getCalculations`](getCalculations.md).

- aggregate:

  Boolean indicating whether output data aggregation should be performed
  or not, "GLO" (or "glo") for aggregation to one global region,
  "REG+GLO" (or "regglo") for a combination of regional and global data.

- file:

  A file name. If given the output is written to that file in the
  outputfolder as specified in the config.

- years:

  A vector of years that should be returned. If set to NULL all
  available years are returned.

- round:

  Number of decimal places to round to. Ignored if `NULL`. See
  [`round()`](https://rdrr.io/r/base/Round.html) for details.

- signif:

  Number of significant digits to round to. Ignored if `NULL`. See
  [`signif()`](https://rdrr.io/r/base/Round.html) for details.

- supplementary:

  boolean deciding whether supplementary information such as weight
  should be returned or not. If set to TRUE a list of elements will be
  returned!

- append:

  boolean deciding whether the output data should be appended in the
  existing file. Works only when a file name is given in the function
  call.

- warnNA:

  boolean deciding whether NAs in the data set should create a warning
  or not

- na_warning:

  deprecated, please use `warnNA` instead

- try:

  if set to TRUE the calculation will only be tried and the script will
  continue even if the underlying calculation failed. If set to FALSE
  calculation will stop with an error in such a case. This setting will
  be overwritten by the global setting debug=TRUE, in which try will be
  always interpreted as TRUE.

- regionmapping:

  alternative regionmapping to use for the given calculation. It will
  temporarily overwrite the global setting just for this calculation.

- writeArgs:

  a list of additional, named arguments to be supplied to the
  corresponding write function

- temporalmapping:

  to aggregate yearly values in the produced magpie object to period
  values. A data frame consisting of the columns 'period', 'year' and
  'weight'. Years in the magpie object will be mapped from years to
  periods as indicated in `temporalmapping` by calculating the weighted
  average using the 'weight' column. Requires magpie object to have
  exactly one temporal sub-dimension.

- ...:

  Additional settings directly forwarded to the corresponding
  calculation function

- outputStatistics:

  a single name of a statistic function ("summary", "sum", or "count")
  or a vector of such names that denote which statistics should be
  computed on the data before aggregation. Disabled by default.

## Value

magpie object with the requested output data either on country or on
regional level depending on the choice of argument "aggregate" or a list
of information if supplementary is set to TRUE.

## Note

The underlying calc-functions are required to provide a list of
information back to `calcOutput`. Following list entries should be
provided:

- **x** - the data itself as magclass object

- **weight** - a weight for the spatial aggregation

- **unit** - unit of the provided data

- **description** - a short description of the data

- **note** (optional) - additional notes related to the data

- **class** (optional \| default = "magpie") - Class of the returned
  object. If set to something other than "magpie" most functionality,
  such as aggregation or unit tests will not be available and is
  switched off!

- **isocountries** (optional \| default = TRUE (mostly) or FALSE (if
  global)) - a boolean indicating whether data is in iso countries or
  not (the latter will deactivate several features such as aggregation)

- **mixed_aggregation** (optional \| default = FALSE) - boolean which
  allows for mixed aggregation (weighted mean mixed with summations). If
  set to TRUE weight columns filled with NA will lead to summation,
  otherwise they will trigger an error.

- **min** (optional) - Minimum value which can appear in the data. If
  provided calcOutput will check whether there are any values below the
  given threshold and warn in this case

- **max** (optional) - Maximum value which can appear in the data. If
  provided calcOutput will check whether there are any values above the
  given threshold and warn in this case

- **structure.spatial** (optional) - regular expression describing the
  name structure of all names in the spatial dimension (e.g.
  `"^[A-Z]\{3\}$"`). Names will be checked against this regular
  expression and disagreements will be reported via a warning.

- **structure.temporal** (optional) - regular expression describing the
  name structure of all names in the temporal dimension (e.g.
  `"^y[0-9]\{4\}$"`). Names will be checked against this regular
  expression and disagreements will be reported via a warning.

- **structure.data** (optional) - regular expression describing the name
  structure of all names in the data dimension (e.g.
  `"^[a-z]*\\\\.[a-z]*$"`). Names will be checked against this regular
  expression and disagreements will be reported via a warning.

- **aggregationFunction** (optional \| default = toolAggregate) -
  Function to be used to aggregate data from country to regions. The
  function must have the argument `x` for the data itself and `rel` for
  the relation mapping between countries and regions and must return the
  data as magpie object in the spatial resolution as defined in rel.

- **aggregationArguments** (optional) - List of additional, named
  arguments to be supplied to the aggregation function. In addition to
  the arguments set here, the function will be supplied with the
  arguments `x`, `rel` and if provided/deviating from the default also
  `weight` and `mixed_aggregation`.

- **putInPUC** (optional) boolean which decides whether this calculation
  should be added to a puc file which contains non-aggregated data and
  can be used to later on aggregate the data to resolutions of own
  choice. If not set `calcOutput` will try to determine automatically,
  whether a file is being required for the puc file or not, but in more
  complex cases (e.g. if calculations below top-level have to be run as
  well) this setting can be used to manually tweak the puc file list.
  CAUTION: Incorrect settings will cause corrupt puc files, so use this
  setting with extreme care and only if necessary.

- **cache** (optional) boolean which decides whether a cache file should
  be written (if caching is active) or not. Default setting is TRUE.
  This can be for instance useful, if the calculation itself is quick,
  but the corresponding file sizes are huge. Or if the caching for the
  given data type does not support storage in RDS format. CAUTION:
  Deactivating caching for a data set which should be part of a PUC file
  will corrupt the PUC file. Use with care.

- **clean_magpie** (optional) boolean, if set to FALSE
  magclass::clean_magpie will not be run on the result

## See also

[`setConfig`](setConfig.md), [`calcTauTotal`](calcTauTotal.md),

## Author

Jan Philipp Dietrich, Patrick Rein

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput(type = "TauTotal")
} # }
```
