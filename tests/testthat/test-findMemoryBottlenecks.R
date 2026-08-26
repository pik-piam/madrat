# nolint start: quotes_linter
# Two consecutive retrieveData runs, mirroring the structure logMultiple uses in
# test-findBottlenecks.R: interspersed NOTE/cache lines that must be ignored, a type ("Yields")
# called twice to exercise cumulative aggregation, and one negative growth value.
memoryLog <- c(
  'Run retrieveData(model = "CellularMAgPIE", rev = 1)',
  'NOTE: ',
  'Run calcOutput(type = "Cluster", aggregate = FALSE)',
  '~  - loading cache calcCluster-F123.rds',
  'Exit calcOutput(type = "Cluster", aggregate = FALSE) in 3 seconds',
  '[memory] calcOutput(type = "Cluster", aggregate = FALSE): peak 500 MB | start 100 MB | end 200 MB | growth 100 MB',
  'Run calcOutput(type = "Yields", aggregate = FALSE)',
  'Exit calcOutput(type = "Yields", aggregate = FALSE) in 30 seconds',
  '[memory] calcOutput(type = "Yields", aggregate = FALSE): peak 4000 MB | start 200 MB | end 900 MB | growth 700 MB',
  'Run calcOutput(type = "Yields", aggregate = FALSE, extra = 2)',
  'Exit calcOutput(type = "Yields", aggregate = FALSE, extra = 2) in 5 seconds',
  '[memory] calcOutput(type = "Yields", aggregate = FALSE, extra = 2): peak 1200 MB | start 900 MB | end 850 MB | growth -50 MB', # nolint: line_length_linter.
  'Exit retrieveData(model = "CellularMAgPIE", rev = 1) in 45 seconds',
  'Run retrieveData(model = "Validation", rev = 1)',
  'Run calcOutput(type = "ValidGridLand", aggregate = FALSE)',
  'Exit calcOutput(type = "ValidGridLand", aggregate = FALSE) in 2 seconds',
  '[memory] calcOutput(type = "ValidGridLand", aggregate = FALSE): peak 300 MB | start 50 MB | end 60 MB | growth 10 MB',
  'Run calcOutput(type = "ValidCroparea", aggregate = FALSE)',
  'Exit calcOutput(type = "ValidCroparea", aggregate = FALSE) in 8 seconds',
  '[memory] calcOutput(type = "ValidCroparea", aggregate = FALSE): peak 2000 MB | start 60 MB | end 500 MB | growth 440 MB',
  'Exit retrieveData(model = "Validation", rev = 1) in 20 seconds'
)
# nolint end

test_that("findMemoryBottlenecks separates blocks and aggregates repeated calls cumulatively", {
  msgs <- capture_messages({
    x <- findMemoryBottlenecks(memoryLog)
  })
  expect_named(x, c("modelCellularMAgPIE", "modelValidation"))
  expect_match(msgs, "Peak memory \\(modelCellularMAgPIE\\): 4000 MB \\| total growth: 750 MB", all = FALSE)
  expect_match(msgs, "Peak memory \\(modelValidation\\): 2000 MB \\| total growth: 450 MB", all = FALSE)

  cell <- x[["modelCellularMAgPIE"]]
  expect_setequal(cell$type, c("Cluster", "Yields"))
  yields <- cell[cell$type == "Yields", ]
  expect_equal(yields$calls, 2)
  expect_equal(yields$"peak[MB]", 4000)   # max across the two calls, not their sum
  expect_equal(yields$"growth[MB]", 650)  # sum across the two calls: 700 + (-50)

  # sorted by peak, highest first, with peak[%] relative to the block's own maximum
  expect_equal(cell$type[1], "Yields")
  expect_equal(cell$"peak[%]"[1], 100)
  expect_equal(cell$"peak[%]"[cell$type == "Cluster"], round(500 / 4000 * 100, 2))

  # no leakage of rows between blocks
  valid <- x[["modelValidation"]]
  expect_setequal(valid$type, c("ValidGridLand", "ValidCroparea"))
  expect_false("Cluster" %in% valid$type)
})

test_that("cumulative = FALSE keeps one row per call including start/end", {
  x <- suppressMessages(findMemoryBottlenecks(memoryLog, cumulative = FALSE))
  cell <- x[["modelCellularMAgPIE"]]
  expect_equal(nrow(cell), 3)
  expect_true(all(c("start[MB]", "end[MB]") %in% names(cell)))
  expect_false("calls" %in% names(cell))
  # the two Yields calls stay separate rows, including the negative growth
  expect_equal(cell$"growth[MB]"[cell$"peak[MB]" == 1200], -50)
})

test_that("unit = \"GB\" converts and renames the memory columns", {
  x <- suppressMessages(findMemoryBottlenecks(memoryLog, unit = "GB"))
  cell <- x[["modelCellularMAgPIE"]]
  expect_true(all(c("peak[GB]", "growth[GB]") %in% names(cell)))
  expect_false(any(grepl("[MB]", names(cell), fixed = TRUE)))
  expect_equal(cell$"peak[GB]"[cell$type == "Yields"], round(4000 / 1024, 2))
})

test_that("findMemoryBottlenecks warns if the log has no memory profiling information", {
  noMemoryLog <- c("Run retrieveData(a = 1)", "Exit retrieveData(a = 1) in 1 seconds")
  expect_warning(findMemoryBottlenecks(noMemoryLog), "memoryProfiling")
})

test_that("findMemoryBottlenecks reports a standalone entry for calls outside any retrieveData", {
  # mirrors logMixed in test-findBottlenecks.R: standalone calls before, between and after a
  # retrieveData call, all carrying memory profiling information
  mixedMemoryLog <- c(
    'Exit calcOutput(type = "Lead", aggregate = FALSE) in 3 seconds',
    '[memory] calcOutput(type = "Lead", aggregate = FALSE): peak 50 MB | start 10 MB | end 20 MB | growth 10 MB', # nolint: line_length_linter.
    'Run retrieveData(model = "A", rev = 1)',
    'Run calcOutput(type = "Inner", aggregate = FALSE)',
    'Exit calcOutput(type = "Inner", aggregate = FALSE) in 2 seconds',
    '[memory] calcOutput(type = "Inner", aggregate = FALSE): peak 100 MB | start 10 MB | end 20 MB | growth 10 MB', # nolint: line_length_linter.
    'Exit retrieveData(model = "A", rev = 1) in 9 seconds',
    'Exit calcOutput(type = "Trail", aggregate = FALSE) in 1 seconds',
    '[memory] calcOutput(type = "Trail", aggregate = FALSE): peak 30 MB | start 5 MB | end 15 MB | growth 10 MB' # nolint: line_length_linter.
  )
  x <- suppressMessages(findMemoryBottlenecks(mixedMemoryLog))
  expect_named(x, c("modelA", "standalone"))
  expect_setequal(x[["standalone"]]$type, c("Lead", "Trail"))
  expect_setequal(x[["modelA"]]$type, "Inner")
})

test_that("findMemoryBottlenecks skips retrieveData blocks without memory information", {
  mixedLog <- c("Run retrieveData(a = 1)",
                "Exit retrieveData(a = 1) in 1 seconds",
                "Run retrieveData(b = 2)",
                "Run calcOutput(type = \"X\", aggregate = FALSE)",
                "Exit calcOutput(type = \"X\", aggregate = FALSE) in 1 seconds",
                "[memory] calcOutput(type = \"X\", aggregate = FALSE): peak 10 MB | start 5 MB | end 8 MB | growth 3 MB", # nolint: line_length_linter.
                "Exit retrieveData(b = 2) in 2 seconds")
  expect_warning({
    x <- findMemoryBottlenecks(mixedLog)
  }, "a1")
  expect_named(x, "b2")
})
