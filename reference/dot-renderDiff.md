# Renders a list of differences based on the relevance. The reasoning is that changed entries should come first, as they can point out subtle problems. Added or removed calls or entries are less important, as they require explicit edits to the pipeline to occur. 1. Changed entries 2. Changed calls 2.1. Added entries 2.2. Removed entries 3. Added calls 4. Removed calls

Renders a list of differences based on the relevance. The reasoning is
that changed entries should come first, as they can point out subtle
problems. Added or removed calls or entries are less important, as they
require explicit edits to the pipeline to occur. 1. Changed entries 2.
Changed calls 2.1. Added entries 2.2. Removed entries 3. Added calls 4.
Removed calls

## Usage

``` r
.renderDiff(
  oldLog,
  newLog,
  diffList,
  sections = c("changedStatistics", "changedCalls", "addedCalls", "removedCalls")
)
```

## Arguments

- oldLog:

  the old log

- newLog:

  the new log

- diffList:

  the list of differences as created by `.compareStatusLogsStatistics`

- sections:

  a vector of section names to be displayed (valid values are:
  "changedStatistics", "changedCalls", "addedCalls", "removedCalls"); by
  default all sections are shown
