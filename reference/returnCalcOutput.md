# Construct a list with the information required for calc-functions to return

A simple helper to have auto-completion for required and optional
elements of calcOutput() retrun lists.

## Usage

``` r
returnCalcOutput(
  x,
  weight,
  unit,
  description,
  note,
  class,
  isocountries,
  mixed_aggregatio,
  min,
  max,
  structure.spatial,
  structure.temporal,
  structure.data,
  aggregationFunction,
  aggregationArguments,
  putinPUC,
  cache
)
```

## Arguments

- x:

  the data itself as magclass object

- weight:

  a weight for the spatial aggregation

- unit:

  unit of the provided data

- description:

  a short description of the data

- note:

  (optional) additional notes related to the data

- class:

  (optional \| default = "magpie") Class of the returned object. If set
  to something other than "magpie" most functionality, such as
  aggregation or unit tests will not be available and is switched off!

- isocountries:

  (optional \| default = TRUE (mostly) or FALSE (if global)) a boolean
  indicating whether data is in iso countries or not (the latter will
  deactivate several features such as aggregation)

- mixed_aggregatio:

  (optional \| default = FALSE) boolean which allows for mixed
  aggregation (weighted mean mixed with summations). If set to TRUE
  weight columns filled with NA will lead to summation, otherwise they
  will trigger an error.

- min:

  (optional) Minimum value which can appear in the data. If provided
  calcOutput will check whether there are any values below the given
  threshold and warn in this case

- max:

  (optional) Maximum value which can appear in the data. If provided
  calcOutput will check whether there are any values above the given
  threshold and warn in this case

- structure.spatial:

  (optional) regular expression describing the name structure of all
  names in the spatial dimension (e.g. "^\[A-Z\]{3}\$"). Names will be
  checked against this regular expression and disagreements will be
  reported via a warning.

- structure.temporal:

  (optional) regular expression describing the name structure of all
  names in the temporal dimension (e.g. "^y\[0-9\]{4}\$"). Names will be
  checked against this regular expression and disagreements will be
  reported via a warning.

- structure.data:

  (optional) regular expression describing the name structure of all
  names in the data dimension (e.g. "^\[a-z\]\*\\.\[a-z\]\*\$"). Names
  will be checked against this regular expression and disagreements will
  be reported via a warning.

- aggregationFunction:

  (optional \| default = toolAggregate) Function to be used to aggregate
  data from country to regions. The function must have the argument x
  for the data itself and rel for the relation mapping between countries
  and regions and must return the data as magpie object in the spatial
  resolution as defined in rel.

- aggregationArguments:

  (optional) List of additional, named arguments to be supplied to the
  aggregation function. In addition to the arguments set here, the
  function will be supplied with the arguments x, rel and if
  provided/deviating from the default also weight and mixed_aggregation.

- putinPUC:

  (optional) boolean which decides whether this calculation should be
  added to a puc file which contains non-aggregated data and can be used
  to later on aggregate the data to resolutions of own choice. If not
  set calcOutput will try to determine automatically, whether a file is
  being required for the puc file or not, but in more complex cases
  (e.g. if calculations below top-level have to be run as well) this
  setting can be used to manually tweak the puc file list. CAUTION:
  Incorrect settings will cause corrupt puc files, so use this setting
  with extreme care and only if necessary.

- cache:

  (optional) boolean which decides whether a cache file should be
  written (if caching is active) or not. Default setting is TRUE. This
  can be for instance useful, if the calculation itself is quick, but
  the corresponding file sizes are huge. Or if the caching for the given
  data type does not support storage in RDS format. CAUTION:
  Deactivating caching for a data set which should be part of a PUC file
  will corrupt the PUC file. Use with care.

## Value

A list with the specified arguments.

## See also

calcOutput

## Examples

``` r
calcSomething <- function() {
  x <- "some calculation"

  returnCalcOutput(x = x, weight = NULL, unit = "a unit",
                   description = "the description")
}
```
