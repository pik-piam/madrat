# toolCompareStatusLogs

Compares status logs and returns a textual rendering of the diff between
the two.

## Usage

``` r
toolCompareStatusLogs(
  oldArchivePath = NULL,
  newArchivePath = NULL,
  oldLogPath = file.path(".", "status.log"),
  newLogPath = file.path(".", "status.log"),
  sections = c("changedStatistics", "changedCalls", "addedCalls", "removedCalls")
)
```

## Arguments

- oldArchivePath:

  Paths to the tgz-archive that serves as the old / new archive. If
  given, oldLogPath / newLogPath determines the name of the log file
  within the archive. If not given, oldLogPath / newLogPath need to be
  given.

- newArchivePath:

  see above

- oldLogPath:

  Paths to a log file, either within an archive, if an archive is given,
  or to a single file.

- newLogPath:

  see above

- sections:

  Vector of sections the output should include. Valid section names are:
  changedStatistics, changedCalls, addedCalls, removedCalls

## Value

A printable string describing the changes that occurred between the old
and the new log.

## Author

Patrick Rein
