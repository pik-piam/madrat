# nolint start: quotes_linter
# A log mixing standalone calls (before, between, after) with two retrieveData blocks, one of
# which uses memory profiling, plus a NOTE/cache line and a "Run"/"Exit retrieveData" marker
# wrapped across lines, mirroring the fixtures used in test-findBottlenecks.R.
mixedLog <- c('Exit calcOutput(type = "Lead", aggregate = FALSE) in 3 seconds',
              'Run ',
              'retrieveData(model = "A", rev = 1)',
              'NOTE: ',
              'Run calcOutput(type = "Inner", aggregate = FALSE)',
              '~  - loading cache calcInner-F123.rds',
              'Exit calcOutput(type = "Inner", aggregate = FALSE) in 2 seconds',
              '[memory] calcOutput(type = "Inner", aggregate = FALSE): peak 100 MB | start 10 MB | end 20 MB | growth 10 MB', # nolint: line_length_linter.
              'Exit retrieveData(model = "A", rev = 1) in 9 seconds',
              'Exit calcOutput(type = "Trail", aggregate = FALSE) in 1 seconds')

test_that(".parseMadratLog drops non-record lines and fills in the expected columns", {
  x <- .parseMadratLog(mixedLog)
  # NOTE/cache lines carry no call information and are dropped: 7 records remain (4 exit, 2 run,
  # 1 memory), not 9 raw lines
  expect_equal(nrow(x), 7)
  expect_setequal(x$marker, c("exit", "run", "memory"))
  expect_true(all(c("level", "class", "type", "marker", "time[s]",
                    "peak[MB]", "start[MB]", "end[MB]", "growth[MB]",
                    "block", "blockType") %in% names(x)))

  # retrieveData rows are forced to level -1
  expect_equal(x$level[x$class == "retrieve"], c(-1, -1))
  # runtime is only present on Exit records
  expect_true(all(is.na(x$"time[s]"[x$marker == "run"])))
  expect_equal(x$"time[s]"[x$type == "Inner" & x$marker == "exit"], 2)
  # memory numbers are only present on [memory] records
  expect_equal(x$"peak[MB]"[x$marker == "memory"], 100)
  expect_true(all(is.na(x$"peak[MB]"[x$marker != "memory"])))
})

test_that(".parseMadratLog assigns block membership, including leading/trailing standalone rows", {
  x <- .parseMadratLog(mixedLog)
  expect_true(is.na(x$block[x$type == "Lead"]))
  expect_true(is.na(x$block[x$type == "Trail"]))
  expect_false(any(is.na(x$block[x$type == "Inner"])))
  expect_equal(unique(x$blockType[x$type == "Inner"]), "modelA")
})

test_that(".splitLogByRetrieve names segments by retrieveData type and adds a standalone entry", {
  x <- .parseMadratLog(mixedLog)
  segments <- .splitLogByRetrieve(x)
  expect_named(segments, c("modelA", "standalone"))
  expect_setequal(segments[["modelA"]]$type, c("modelA", "Inner"))
  expect_setequal(segments[["standalone"]]$type, c("Lead", "Trail"))
})

test_that(".splitLogByRetrieve omits the standalone entry when there is nothing outside a block", {
  x <- .parseMadratLog(c('Run retrieveData(model = "A", rev = 1)',
                         'Exit retrieveData(model = "A", rev = 1) in 1 seconds'))
  segments <- .splitLogByRetrieve(x)
  expect_named(segments, "modelA")
})

test_that(".retrieveDataBlocks starts a block right after the previous close if Run is missing", {
  # mirrors the "log" fixture in test-findBottlenecks.R, which has no "Run retrieveData(" line
  x <- .parseMadratLog(c('Exit calcOutput(type = "X", aggregate = FALSE) in 1 seconds',
                         'Exit retrieveData(model = "A", rev = 1) in 2 seconds'))
  expect_equal(unique(x$block), 1)
  expect_equal(unique(x$blockType), "modelA")
})
# nolint end
