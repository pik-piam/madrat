# Data caching in madrat

A central feature of the madrat framework is its ability to load data
from cache rather than recompute it when the calculation have run
already before. Here, we explain what user should know about the caching
to avoid unwanted behavior.

## Basics

By default every read- or calc-function creates a cache file from its
computations and stores it in the cachefolder. Where this folder is
located can be checked via

``` r
library(madrat, quietly = TRUE)
#> 
#> Attaching package: 'magclass'
#> The following objects are masked from 'package:base':
#> 
#>     pmax, pmin
getConfig("cachefolder", verbose = FALSE)
#> [1] "/tmp/RtmpQPRtvq/madrat/cache/default"
```

When running data processing via `retrieveData` it currently offers two
types of cache folders: `cachetype = "def"` will use a shared
cachefolder in which all processes write their cache files by default.
RetrieveData will check in this folder for fitting cache files and read
them if available. Whether they are fitting or not will depend on their
`fingerprint` which is explained further down. With `cachetype = "rev"`
`retrieveData` will create a new, revision-specific cachefolder and set
`setConfig(forcecache = TRUE)` (default is FALSE). Via this approach
calculations will start with new cache files at all but created cache
files will be read if a calculation is repeated. The forcecache option
will in this case make sure that any available cache file which fits the
function call is read in, independent of whether the content of the
cache file might be outdated or not.

## Cache format

By default cache files are written as gzip-compressed `rds` files. Since
serializing and compressing the data can make up a significant share of
the runtime of a data processing, the format can be changed via
`setConfig(cacheformat = ...)`. Besides `"rds"` madrat ships support for
`"qs2"`, which is considerably faster but requires the `qs2` package:

``` r
setConfig(cacheformat = "qs2")
```

Alternatively the format can be set via the environment variable
`MADRAT_CACHEFORMAT`, which is the more convenient option if a whole
compute cluster should use the same setting.

Settings of the format itself are not managed by madrat, but by the
corresponding package. For `qs2` the number of threads and the
compression level are set via its own options, e.g. in your `.Rprofile`:

``` r
qs2::qopt("nthreads", 4)
qs2::qopt("compress_level", 3)
```

The `cachecompression` setting only applies to the format `"rds"`.

### Switching the format

Changing the cache format does not invalidate an existing cache: madrat
looks for a cache file in the configured format first, and falls back to
an already existing `rds` file if there is none. Such a file is only
read, never rewritten, so no cache regeneration is triggered and nothing
is written behind your back.

Please note what this implies: parts of your processing which do not
change keep reading their existing `rds` cache files. Writing becomes
faster immediately, but reading only becomes faster for those cache
files which have been written anew, e.g. because the fingerprint of the
corresponding function changed.

It is safe to run processings with different cache formats on the same
cache folder, as the file extension is part of the cache file name. Be
aware though that a processing using `"rds"` cannot read cache files in
another format, so such a setup will over time store the same data
twice.

### Other formats

Further formats can be added from other packages via
`registerCacheFormat`, which expects functions to read and write a file:

``` r
registerCacheFormat("qs", write = qs::qsave, read = qs::qread)
```

The registration should happen in the `.onLoad` function of the
providing package, so that the format is also available in the separate
R sessions which madrat starts for some operations (e.g. in
`pucAggregate`).

Whether a format actually works is not checked when it is registered or
configured. If reading or writing a cache file fails, e.g. because a
required package is not installed, this is reported in the log together
with the reason, but it does not stop a calculation. Be aware that this
also means that a broken format leads to no cache files being written at
all, so watch out for such messages if a processing is unexpectedly
slow.

## Fingerprint

In order to estimate whether a calculation should be rerun or whether
the data can be read from cache madrat creates fingerprints for each
function. If the fingerprint of the current function call agrees with
the fingerprint of the corresponding cache file the cache is assumed
up-to-date and read in. If they disagree, the cache file is assumed to
be potentially outdated and ignored (except for `forcecache = TRUE` in
which case it would be read in anyways).

The fingerprint is created by looking at the dependency graph of a
function which can be retrieved via `getDependencies`:

``` r
getDependencies("calcTauTotal", packages = "madrat")
#>                        func type package
#> 1                   readTau read  madrat
#> 2         toolSubtypeSelect tool  madrat
#> 3             toolAggregate tool  madrat
#> 4           toolCountryFill tool  madrat
#> 5  toolGetAggregationMatrix tool  madrat
#> 6     toolAggregateWeighted tool  madrat
#> 7   toolAggregateUnweighted tool  madrat
#> 8            toolGetMapping tool  madrat
#> 9            toolZeroWeight tool  madrat
#> 10            toolExpandRel tool  madrat
#> 11            toolFixWeight tool  madrat
#> 12           toolMapFromRel tool  madrat
#>                                 call     hash
#> 1                   madrat:::readTau 51d42a7b
#> 2         madrat:::toolSubtypeSelect 19a011f1
#> 3             madrat:::toolAggregate d87bab1e
#> 4           madrat:::toolCountryFill a1152c65
#> 5  madrat:::toolGetAggregationMatrix d29738b5
#> 6     madrat:::toolAggregateWeighted 8bb41003
#> 7   madrat:::toolAggregateUnweighted 255c73b9
#> 8            madrat:::toolGetMapping b688b718
#> 9            madrat:::toolZeroWeight 17e8a260
#> 10            madrat:::toolExpandRel 86691784
#> 11            madrat:::toolFixWeight ec516307
#> 12           madrat:::toolMapFromRel 37c2d9ee
```

The dependency graph lists all calls of `calc`, `read` and `tool`
functions a function depends on (not only calls in the function itself,
but also calls in the functions which have been called in order to run
the function). The fingerprinting function creates hashes of all these
functions and all source folders involved in this process and combines
them to one single hash which is the fingerprint of that specific
function:

``` r
setConfig(verbosity = 3)
#> Global configuration update:
#>   verbosity: 1 -> 3
fp <- madrat:::fingerprint("calcTauTotal")
#> hash components (201f3fe1):
#>   49fe8440 | madrat:::calcTauTotal | madrat:::calcTauTotal
#>   51d42a7b | madrat:::readTau | madrat:::readTau
#>   c095ab28 | madrat:::sysdata$iso_cell | madrat:::sysdata$iso_cell
#>   d87bab1e | madrat:::toolAggregate | madrat:::toolAggregate
#>   255c73b9 | madrat:::toolAggregateUnweighted | 
#> madrat:::toolAggregateUnweighted
#>   8bb41003 | madrat:::toolAggregateWeighted | 
#> madrat:::toolAggregateWeighted
#>   a1152c65 | madrat:::toolCountryFill | madrat:::toolCountryFill
#>   86691784 | madrat:::toolExpandRel | madrat:::toolExpandRel
#>   ec516307 | madrat:::toolFixWeight | madrat:::toolFixWeight
#>   d29738b5 | madrat:::toolGetAggregationMatrix | 
#> madrat:::toolGetAggregationMatrix
#>   b688b718 | madrat:::toolGetMapping | madrat:::toolGetMapping
#>   37c2d9ee | madrat:::toolMapFromRel | madrat:::toolMapFromRel
#>   19a011f1 | madrat:::toolSubtypeSelect | madrat:::toolSubtypeSelect
#>   17e8a260 | madrat:::toolZeroWeight | madrat:::toolZeroWeight
#>   3dd304aa | magclass:::ncells | magclass:::ncells
```

As a hash has the characteristic to change when its input changes, an
unchanged hash means that also the respective function or source folder
did not change. Hence, an identical fingerprint means that the involved
functions and source data did not change. So, if the fingerprint of the
cache file agrees with the fingerprint calculated for the calculation it
is quite likely that the data contained in the cache file also agrees
with the output of the calculation one would run it again.

The reason why it is only quite likely but not certain is that not all
parts of the calculation are covered: The dependency graph only
considers madrat-style functions, e.g. functions not starting with
`download`, `read`, `correct`, `convert`, `calc`, or `tool` will not be
considered. In many cases this should be ok, considering that external
functions used in the calculation will likely keep their behavior over
time, but there might be instances in which this assumption is violated
(e.g. if parts of the calculation are outsourced in a function not
following these conventions).

On the other hand, the dependency graph might also include dependencies
which only exist on the paper, as it does only scan for calls of the
corresponding functions in the code, but cannot interpret which calls
are actually be computed for a given calculation, e.g. there could be if
clauses in a calc-function selecting different source data types. The
dependency graph will show a dependency to all sources even if only one
of these sources might be used at the end.

## Customize fingerprinting

To make sure that the fingerprint is appropriately reflecting the
current status of a calculation there are a few possibilities to steer
its behavior:

1.  Use madrat-style functions for all calculation that should get
    monitored by the fingerprinting algorithm (e.g. if part of the
    calculation is outsourced, call this new function `tool..` to have
    it monitored.)

2.  Adjust the fingerprinting via control flags for all other cases.

## Control flags

Control flags can be used to manually include or exclude functions in
the fingerprinting. Control flags are comments in the functions which
are put in quotes and start with `!#`. They can look like:

``` r
"!# @monitor madrat:::sysdata magclass:::ncells"
"!# @ignore  madrat:::toolAggregate"
```

Each line contains a control flag starting with the flag name (here
`monitor` or `ignore`) and afterwards with the arguments of this control
flag. The `monitor` flag specifies calls which should get monitored in
addition to the ones anyway monitored (in the example the `sysdata`
object in `madrat` and the `ncells` functions of the `magclass` package
are additionally being monitored). The `ignore` flag specifies which
calls should not be monitored even so `getDepenendencies` says
otherwise.

While the `ignore` statement has to be mentioned explicitly for each
function, the `monitor` statement will be passed on automatically to all
subsequent functions (e.g. if a read function has a monitor statement
all calc-functions used that read function will also monitor the
additional calls of that statement, but in the same example the ignore
statement would only be used for the read function itself).

In particular the `ignore` statement has to be handled with care as a
wrong information here might lead to outdated cache files being read in.
So, only use it if really necessary and if you know exactly what you are
doing.

## Examples

``` r
setConfig(globalenv = TRUE)
#> Global configuration update:
#>   globalenv: FALSE -> TRUE
readData <- function() return(1)
readData2 <- function() return(2)
calcExample <- function() {
  a <- readSource("Data")
  return(a)
}
calcExample2 <- function() {
  a <- readSource("Data")
  if (FALSE) a <- readSource("Data2")
  return(a)
}
```

In this example are two source data sets and two calculation functions.
`calcExample` only depends on `readData` while `calcExample2` depends on
both data sources.

``` r
fp <- madrat:::fingerprint("calcExample")
#> hash components (d927f460):
#>   741a3677 | calcExample | calcExample
#>   783a5e2f | readData | readData
fp2 <- madrat:::fingerprint("calcExample2")
#> hash components (81a4a47d):
#>   73001063 | calcExample2 | calcExample2
#>   783a5e2f | readData | readData
#>   fb52578f | readData2 | readData2
```

Looking at the fingerprints this is reflected in the hash components of
each fingerprint (please NOTE that the source folders are not hashed in
this example as they do not exist yet. If they exist they would show up
here as well as hash components). One can see, that the hash for
`readData` is the same in both fingerprints but as the other hashes
differ also the resulting fingerprint for both calculations is
different.

``` r
readData <- function() return(99)
fp <- madrat:::fingerprint("calcExample")
#> hash components (1fd8c70c):
#>   741a3677 | calcExample | calcExample
#>   06f7b7ad | readData | readData
fp2 <- madrat:::fingerprint("calcExample2")
#> hash components (f119542e):
#>   73001063 | calcExample2 | calcExample2
#>   06f7b7ad | readData | readData
#>   fb52578f | readData2 | readData2
```

Changing the `readData` function changes the hash of this function and
thereby also the fingerprints of both calc functions even so the hash of
the calc functions itself did not change.

``` r
readData2 <- function() {
  "!# @monitor madrat:::toolAggregate"
  return(99)
}
fp <- madrat:::fingerprint("calcExample")
#> hash components (1fd8c70c):
#>   741a3677 | calcExample | calcExample
#>   06f7b7ad | readData | readData
fp2 <- madrat:::fingerprint("calcExample2")
#> hash components (6018afee):
#>   73001063 | calcExample2 | calcExample2
#>   d87bab1e | madrat:::toolAggregate | madrat:::toolAggregate
#>   06f7b7ad | readData | readData
#>   13c681da | readData2 | readData2
```

Adding a monitor control flag in `readData` also add this hash component
to all subsequent fingerprint calculations.

``` r

calcExample2 <- function() {
  "!# @ignore readData2"
  a <- readSource("Data")
  if (FALSE) a <- readSource("Data2")
  return(a)
}

calcExample3 <- function() {
  a <- calcOutput("Example2")
  return(a)
}

fp2 <- madrat:::fingerprint("calcExample2")
#> hash components (18137f33):
#>   2da80525 | calcExample2 | calcExample2
#>   d87bab1e | madrat:::toolAggregate | madrat:::toolAggregate
#>   06f7b7ad | readData | readData
fp3 <- madrat:::fingerprint("calcExample3")
#> hash components (b542a529):
#>   2da80525 | calcExample2 | calcExample2
#>   d51ac46e | calcExample3 | calcExample3
#>   d87bab1e | madrat:::toolAggregate | madrat:::toolAggregate
#>   06f7b7ad | readData | readData
#>   13c681da | readData2 | readData2
```

The `ignore` flag in `calcExample2` excludes `readData2` from the
fingerprint calculation. But in contrast to the `monitor` statement this
information is not forwarded to `calcExample3`. Hence, the latter does
not only monitor `madrat:::toolAggregate` but also `readData2`!

## forcecache

Before the introduction of fingerprinting forcing the use of cache files
was the default approach. However, in the new setup the argument
`forcecache = TRUE` should only be used under very specific
circumstances, as it does not guarantee that the data agrees with the
code of the corresponding package. In particular production runs should
always use `forcecache = FALSE`.

A scenario in which `forcecache = TRUE` might still make sense are
development cases in which up-to-date inputs are not required for proper
function development. In these cases development can be speed up by
using potentially outdated cache files as a starting point to avoid
lengthy calculations of parts irrelevant for the current development
stage.

If you are unsure what to use, always go with `forcecache = FALSE`.
