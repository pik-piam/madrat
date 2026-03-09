# setConfig

This function manipulates the current madrat configuration. In general,
NULL means that the argument remains as it is whereas all other inputs
will overwrite the current setting. For values which can be reset to
NULL (currently only "extramappings") you can achieve a reset by setting
the value to "".

## Usage

``` r
setConfig(
  ...,
  regionmapping = NULL,
  extramappings = NULL,
  packages = NULL,
  globalenv = NULL,
  enablecache = NULL,
  verbosity = NULL,
  mainfolder = NULL,
  sourcefolder = NULL,
  cachefolder = NULL,
  mappingfolder = NULL,
  outputfolder = NULL,
  pucfolder = NULL,
  tmpfolder = NULL,
  nolabels = NULL,
  forcecache = NULL,
  ignorecache = NULL,
  cachecompression = NULL,
  hash = NULL,
  diagnostics = NULL,
  debug = NULL,
  maxLengthLogMessage = NULL,
  redirections = NULL,
  .cfgchecks = TRUE,
  .verbose = TRUE,
  .local = FALSE
)

localConfig(...)
```

## Arguments

- ...:

  Arguments forwarded to setConfig

- regionmapping:

  The name of the csv file containing the region mapping that should be
  used for aggregation (e.g. "regionmappingREMIND.csv").

- extramappings:

  Names of additional mappings supplementing the given region mapping.
  This allows for additional aggregation levels such as subnational
  aggregation.

- packages:

  A character vector with packages in which corresponding read and calc
  functions should be searched for

- globalenv:

  Boolean deciding whether sources/calculations in the global
  environment should be included or not

- enablecache:

  Is deprecated and will be ignored. Please use `ignorecache` instead.

- verbosity:

  an integer value describing the verbosity of the functions (2 = full
  information, 1 = only warnings and execution information, 0 = only
  warnings, -1 = no information)

- mainfolder:

  The mainfolder where all data can be found and should be written to.

- sourcefolder:

  The folder in which all source data is stored (in sub-folders with the
  name of the source as folder name). In the default case this argument
  is set to NA meaning that the default folder should be used which is
  \<mainfolder\>/sources

- cachefolder:

  The folder in which all cache files should be written to. In the
  default case this argument is set to NA meaning that the default
  folder should be used which is \<mainfolder\>/cache

- mappingfolder:

  A folder containing all kinds of mappings (spatial, temporal or
  sectoral). In the default case this argument is set to NA meaning that
  the default folder should be used which is \<mainfolder\>/mappings

- outputfolder:

  The folder all outputs should be written to. In the default case this
  argument is set to NA meaning that the default folder should be used
  which is \<mainfolder\>/output

- pucfolder:

  The path where portable unaggregated collection (puc) files are
  located. NA by default, which means \<mainfolder\>/puc

- tmpfolder:

  Path to a temp folder for temporary storage of files. By default set
  to \<mainfolder\>/tmp

- nolabels:

  vector of retrieve models (e.g. "EXAMPLE" in case of "fullEXAMPLE")
  which should NOT apply a replacement of known hashes with given code
  labels

- forcecache:

  Argument that allows to force madrat to read data from cache if the
  corresponding cache files exist. It is either a boolean to fully
  activate or deactivate the forcing or a vector of files (e.g. readTau,
  calcTauTotal) or type (e.g. Tau, TauTotal) that should be read from
  cache in any case.

- ignorecache:

  Argument that allows madrat to ignore the forcecache argument for the
  given vector of files (e.g. readTau, calcTauTotal) or types (e.g. Tau,
  TauTotal) called by calcOutput or readSource. The top level function
  must always be part of this list.

- cachecompression:

  logical or character string specifying whether cache files use
  compression. TRUE corresponds to gzip compression, and character
  strings "gzip", "bzip2" or "xz" specify the type of compression.

- hash:

  specifies the used hashing algorithm. Default is "xxhash32" and all
  algorithms supported by
  [`digest`](https://eddelbuettel.github.io/digest/man/digest.html) can
  be used.

- diagnostics:

  Either FALSE (default) to avoid the creation of additional log files
  or TRUE to write a log of diagnostic outputs to "diagnostics.log".
  Setting diagnostics to a file name is deprecated.

- debug:

  Boolean which activates a debug mode. In debug mode all calculations
  will be executed with try=TRUE so that calculations do not stop even
  if the previous calculation failed. This can be helpful to get a full
  picture of errors rather than only seeing the first one. In addition
  debug=TRUE will add the suffix "debug" to the files created to avoid
  there use in productive runs. Furthermore, with debug=TRUE
  calculations will be rerun even if a corresponding tgz file already
  exists.

- maxLengthLogMessage:

  in log messages evaluated arguments are printed if the resulting
  message is shorter than this value, otherwise arguments are shown as
  passed, potentially with unevaluated variable names

- redirections:

  A list of source folder redirections, intended to be set by
  [`redirectSource`](redirectSource.md). See that function's
  documentation for more details.

- .cfgchecks:

  boolean deciding whether the given inputs to setConfig should be
  checked for consistency or just be accepted (latter is only necessary
  in very rare cases and should not be used in regular cases)

- .verbose:

  boolean deciding whether status information/updates should be shown or
  not

- .local:

  boolean deciding whether options are only changed until the end of the
  current function execution OR environment for which the options should
  get changed.

## Functions

- `localConfig()`: A wrapper for setConfig(..., .local = TRUE)

## Note

`setConfig` must only be used before the data processing is started and
changes in the configuration from within a download-, read-, correct-,
convert-, calc-, or full-function are not allowed! Only allowed
configuration update is to add another `extramapping` via
[`addMapping`](addMapping.md). Currently the use of `setConfig` within
any of these functions will trigger a warning, which is planned to be
converted into an error message in one of the next package updates!

## See also

[`getConfig`](getConfig.md), [`getISOlist`](getISOlist.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
setConfig(forcecache = c("readSSPall", "convertSSPall"))
} # }
```
