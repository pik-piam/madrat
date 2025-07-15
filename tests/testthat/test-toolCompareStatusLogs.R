test_that("compare logs based on archives or single files", {

  withr::local_dir(withr::local_tempdir())

  # Setup two status logs, two archives, one with two status log files
  statusLog1 <- "
  cf():
  - statistic: count
    type: statistic
    data: 5"
  statusLog2 <- "
  cf():
  - statistic: count
    type: statistic
    data: 10"
  statusLog2a <- "
  cf():
  - statistic: count
    type: statistic
    data: 2"

  writeLines(statusLog1, "status.log")
  writeLines(statusLog1, "status-a.log")
  tar("archive1.tgz",
      files = c("./status.log"),
      compression = "gzip")

  writeLines(statusLog2, "status.log") # This overrides the status.log, to simplify the tar creation.
  writeLines(statusLog2a, "status-b.log")
  tar("archive2.tgz",
      files = c("./status.log", "./status-b.log"),
      compression = "gzip")

  expectCorrectDiff <- function(compareResult, diffText) {
    expect_match(compareResult, diffText)
  }

  # Two archives given
  expectCorrectDiff(toolCompareStatusLogs(oldArchivePath = "archive1.tgz",
                                          newArchivePath = "archive2.tgz"),
                    "5 -> 10")

  # Two archives with additional file name
  expectCorrectDiff(toolCompareStatusLogs(oldArchivePath = "archive1.tgz",
                                          newArchivePath = "archive2.tgz", newLogPath = "./status-b.log"),
                    "5 -> 2")

  # Two single files given
  expectCorrectDiff(toolCompareStatusLogs(oldLogPath = "status-a.log",
                                          newLogPath = "status-b.log"),
                    "5 -> 2")

  # One archive and one single file given
  expectCorrectDiff(toolCompareStatusLogs(oldArchivePath = "archive1.tgz",
                                          newLogPath = "status-b.log"),
                    "5 -> 2")

  # No arguments
  expect_match(toolCompareStatusLogs(), "Nothing to do")

})

test_that("output of unchanged logs", {

  withr::local_dir(withr::local_tempdir())

  statusLog <- "
  cf():
  - statistic: count
    type: statistic
    data: 5"
  writeLines(statusLog, "status.log")
  writeLines(statusLog, "status-a.log")

  expect_match(toolCompareStatusLogs(oldLogPath = "status.log",
                                     newLogPath = "status-a.log"),
               "No changes between logs.")

})

.emptyNamedList <- function() {
  emptyNamedList <- list()
  names(emptyNamedList) <- list()
  return(emptyNamedList)
}

test_that("compare of unchanged logs", {
  expectEmptyDiff <- function(old, new) {
    result <- .compareStatusLogsStatistics(old, new)
    expect_length(result[["addedCalls"]], 0)
    expect_length(result[["removedCalls"]], 0)
    expect_length(result[["changedCalls"]], 0)
  }

  expectEmptyDiff(list(), list())
  expectEmptyDiff(list("calcFunc(a=100, b=10)" = list()), list("calcFunc(a=100, b=10)" = list()))
  # Different ordering does not matter
  expectEmptyDiff(list("calcFunc(a=100, b=10)" = list(), "calcFunc2()" = list()),
                  list("calcFunc2()" = list(), "calcFunc(a=100, b=10)" = list()))
  # Also not for more complex data
  summaryStatistic <- list(type = "statistic", statistic = "summary", data = list(min = 0, max = 10, mean = 5.52342))
  summaryStatShuffled <- list(type = "statistic", statistic = "summary", data = list(min = 0, mean = 5.52342, max = 10))
  expectEmptyDiff(list("calcFunc(a=100, b=10)" = list(summaryStatistic)),
                  list("calcFunc(a=100, b=10)" = list(summaryStatShuffled)))
})

test_that("compare of logs with added or removed calls", {
  # Added call
  expect_mapequal(.compareStatusLogsStatistics(list("calcFunction(a=100)" = list()),
                                               list("calcFunction(a=100)" = list(), "calcFunction2(a=100)" = list())),
                  list(addedCalls = list("calcFunction2(a=100)"),
                       removedCalls = list(),
                       changedCalls = .emptyNamedList()))

  # Removed call
  expect_mapequal(.compareStatusLogsStatistics(list("calcFunction(a=100)" = list(), "calcFunction2(a=100)" = list()),
                                               list("calcFunction(a=100)" = list())),
                  list(addedCalls = list(),
                       removedCalls = list("calcFunction2(a=100)"),
                       changedCalls = .emptyNamedList()))
})

.expectChangedCalls <- function(old, new, changedCalls) {
  result <- .compareStatusLogsStatistics(old, new)
  expect_mapequal(result,
                  list(addedCalls = list(), removedCalls = list(),
                       changedCalls = changedCalls))
}

test_that("compare results with changed calls", {
  countStatistic <- list(type = "statistic", statistic = "count", data = 5)
  sumStatistic <- list(type = "statistic", statistic = "sum", data = 10)

  # Added entries
  .expectChangedCalls(list("cf()" = list(countStatistic)),
                      list("cf()" = list(countStatistic, sumStatistic)),
                      list("cf()" = list(addedEntries = list(sumStatistic),
                                         removedEntries = list(),
                                         changedEntries = list())))

  .expectChangedCalls(list("cf()" = list("check 1")),
                      list("cf()" = list("check 1", "check 2")),
                      list("cf()" = list(addedEntries = list("check 2"),
                                         removedEntries = list(),
                                         changedEntries = list())))

  # Removed entries
  .expectChangedCalls(list("cf()" = list(countStatistic, sumStatistic)),
                      list("cf()" = list(countStatistic)),
                      list("cf()" = list(addedEntries = list(),
                                         removedEntries = list(sumStatistic),
                                         changedEntries = list())))

  .expectChangedCalls(list("cf()" = list("check 1", "check 2")),
                      list("cf()" = list("check 1")),
                      list("cf()" = list(addedEntries = list(),
                                         removedEntries = list("check 2"),
                                         changedEntries = list())))

  # Changed statistic

  ## Simple data
  countStatistic2 <- countStatistic
  countStatistic2["data"] <- 10
  .expectChangedCalls(list("cf()" = list(countStatistic)),
                      list("cf()" = list(countStatistic2)),
                      list("cf()" = list(addedEntries = list(),
                                         removedEntries = list(),
                                         changedEntries = list(list(old = countStatistic, new = countStatistic2)))))

  ## Complex data
  summaryStatistic <- list(type = "statistic", statistic = "summary", data = list(min = 0, max = 10, mean = 5.52342))
  summaryStatistic2 <- summaryStatistic
  summaryStatistic2[["data"]]["max"] <- 12
  .expectChangedCalls(list("cf()" = list(summaryStatistic)),
                      list("cf()" = list(summaryStatistic2)),
                      list("cf()" = list(addedEntries = list(),
                                         removedEntries = list(),
                                         changedEntries = list(list(old = summaryStatistic, new = summaryStatistic2)))))

  # Structured entries other than statistics are handled as tokens without identity
  someList <- list(calculation = "sum", payload = list(1, 2, 3))
  someList2 <- list(calculation = "sum", payload = list(4, 5, 6))
  .expectChangedCalls(list("cf()" = list(someList)),
                      list("cf()" = list(someList2)),
                      list("cf()" = list(addedEntries = list(someList2),
                                         removedEntries = list(someList),
                                         changedEntries = list())))

})

test_that("unexpected log data", {
  countStatistic <- list(type = "statistic", statistic = "count", data = 5)
  countStatistic2 <- countStatistic
  countStatistic2["data"] <- 10

  expect_warning(.expectChangedCalls(list("cf()" = list(countStatistic)),
                                     list("cf()" = list(countStatistic2, countStatistic)),
                                     list("cf()" = list(addedEntries = list(),
                                                        removedEntries = list(),
                                                        changedEntries = list(list(old = countStatistic,
                                                                                   new = countStatistic2))))))

})
