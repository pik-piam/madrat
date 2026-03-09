# cacheCleanup

Delete files older than the specified number of days, based on file time
metadata (per default atime = last access time).

## Usage

``` r
cacheCleanup(
  daysThreshold,
  path = getConfig("cachefolder", verbose = FALSE),
  timeType = c("atime", "mtime", "ctime"),
  ask = TRUE,
  readlineFunction = readline
)
```

## Arguments

- daysThreshold:

  Files older than this many days are deleted/returned.

- path:

  Path to where to look for old files.

- timeType:

  Which file metadata time should be used. One of atime (last access
  time, default), mtime (last modify time), ctime (last metadata
  change).

- ask:

  Whether to ask before deleting.

- readlineFunction:

  Only needed for testing. A function to prompt the user for input.

## Value

If the user answers 'n', a data.frame as returned by base::file.info,
containing only files older than \<daysThreshold\> days.

## Details

File time metadata is not available on all systems and the semantics are
also system dependent, so please be careful and check that the correct
files are deleted. This function will return a data.frame containing all
files that would be deleted if the user answers 'n' to the question. If
deleting files fails a warning is created.
