# Single quotes to not have to escape the double quotes in the test string.
# nolint start: quotes_linter
log <- c('~ Run ',
         '~ calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4")',
         '~~ Run readSource("Tau", source)',
         '~~ Exit readSource("Tau", source)',
         '~~ in 5.15 seconds',
         '~ Exit calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4") in 5.56 seconds',
         '~ Exit retrieveData("example", rev = 2) in 5.58 seconds')

logBugged <- c('~ Run calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4")',
               '~~ Run mixSource("Tau", source)',
               '~~ Exit mixSource("Tau", source) in 5.15 seconds',
               '~ Exit calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4") in 5.56 seconds',
               '~ Exit retrieveData("example", rev = 2) in 5.58 seconds')

# Two consecutive retrieveData runs in one log, mirroring the structure of a real diagnostics.log:
# top-level calls have no "~" prefix, sub-calls are "~"-nested, "Run "/"Exit " entries can be split
# across lines, and cache/statistics/NOTE lines are interspersed (and ignored during analysis).
logMultiple <- c('Run retrieveData(model = "CellularMAgPIE", rev = list(c(4, 120)), dev = "test", puc = FALSE)',
                 'NOTE: ',
                 'Start preprocessing for ',
                 'Run calcOutput(type = "Cluster", aggregate = FALSE, ctype = "c200")',
                 '~  - loading cache calcCluster-F5730e8b3-a3c5e272.rds',
                 'Exit calcOutput(type = "Cluster", aggregate = FALSE, ctype = "c200") in 10.42 seconds',
                 'Run ',
                 'calcOutput(type = "Yields", aggregate = FALSE, file = "lpj_yields_0.5.mz")',
                 '~  - loading cache calcYields-Fe5977d47-3494a143.rds',
                 '~ [statistics] summary of calcOutput(type = "Yields", aggregate = FALSE)',
                 'Exit ',
                 'calcOutput(type = "Yields", aggregate = FALSE, file = "lpj_yields_0.5.mz")',
                 ' in 32.87 seconds',
                 'Run calcOutput(type = "AtmosphericDeposition", aggregate = FALSE, cellular = TRUE)',
                 '~ Run calcOutput(type = "LanduseInitialisation", aggregate = FALSE, cellular = TRUE)',
                 '~ Exit calcOutput(type = "LanduseInitialisation", aggregate = FALSE, cellular = TRUE) in 0.31 seconds',
                 'Exit calcOutput(type = "AtmosphericDeposition", aggregate = FALSE, cellular = TRUE) in 1.5 seconds',
                 'Exit retrieveData(model = "CellularMAgPIE", rev = list(c(4, 120)), dev = "test", puc = FALSE) in 45.1 seconds',
                 'Run retrieveData(model = "Validation", rev = list(c(4, 120)), puc = FALSE)',
                 'NOTE: ',
                 'Run calcOutput(type = "ValidGridLand", aggregate = FALSE, file = "land.mz")',
                 '~  - loading cache calcValidGridLand-Fabcdef01.rds',
                 'Exit calcOutput(type = "ValidGridLand", aggregate = FALSE, file = "land.mz") in 7.2 seconds',
                 'Run calcOutput(type = "ValidCroparea", aggregate = "cluster", file = "croparea.mz")',
                 '~ Run readSource("LUH2v2", subtype = "states")',
                 '~ Exit readSource("LUH2v2", subtype = "states") in 4 seconds',
                 'Exit calcOutput(type = "ValidCroparea", aggregate = "cluster", file = "croparea.mz") in 6.6 seconds',
                 'Exit retrieveData(model = "Validation", rev = list(c(4, 120)), puc = FALSE) in 15.3 seconds')

test_that("bottleneck detection works", {
  expect_message({
    x <- findBottlenecks(log)
  }, "5 seconds")
  expect_named(x, "example")
  expect_equal(nrow(x[["example"]]), 3)
  expect_equal(ncol(x[["example"]]), 7)
  file <- tempfile()
  writeLines(logBugged, file)
  expect_warning({
    x <- findBottlenecks(file, unit = "h")
  }, "could not be properly detected")
  expect_named(x, "example")
})

test_that("multiple retrieveData calls are analyzed separately", {
  msgs <- capture_messages({
    x <- findBottlenecks(logMultiple, unit = "s")
  })
  # one named entry and one "Total runtime" message per retrieveData call
  expect_named(x, c("modelCellularMAgPIE", "modelValidation"))
  expect_match(msgs, "Total runtime \\(modelCellularMAgPIE\\)", all = FALSE)
  expect_match(msgs, "Total runtime \\(modelValidation\\)", all = FALSE)
  expect_s3_class(x[["modelCellularMAgPIE"]], "data.frame")
  expect_s3_class(x[["modelValidation"]], "data.frame")

  # each block only contains its own functions (no leakage of rows between blocks)
  expect_setequal(x[["modelCellularMAgPIE"]]$type,
                  c("modelCellularMAgPIE", "Cluster", "Yields", "AtmosphericDeposition", "LanduseInitialisation"))
  expect_setequal(x[["modelValidation"]]$type,
                  c("modelValidation", "ValidGridLand", "ValidCroparea", "LUH2v2"))

  # net runtimes are computed per block: the nested sub-call time is subtracted from its
  # own parent only, and does not leak across blocks
  valid <- x[["modelValidation"]]
  expect_equal(valid$"net[s]"[valid$type == "ValidCroparea"], 6.6 - 4) # parent minus nested readSource
  expect_equal(valid$"net[s]"[valid$type == "ValidGridLand"], 7.2)     # unaffected by the other block
})
# nolint end
