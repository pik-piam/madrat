log <- c('Run calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4")',
         '> Run readSource("Tau", source)',
         '> Exit readSource("Tau", source) in 5.15 seconds',
         'Exit calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4") in 5.56 seconds',
         'Exit retrieveData("example", rev = 2) in 5.58 seconds')

logBugged <- c('Run calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4")',
               '> Run mixSource("Tau", source)',
               '> Exit mixSource("Tau", source) in 5.15 seconds',
               'Exit calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4") in 5.56 seconds',
               'Exit retrieveData("example", rev = 2) in 5.58 seconds')

test_that("bottleneck detection worksy", {
  expect_message({
    x <- findBottlenecks(log)
  }, "5 seconds")
  expect_equal(nrow(x), 3)
  expect_equal(ncol(x), 7)
  file <- tempfile()
  writeLines(logBugged, file)
  expect_warning({
    x <- findBottlenecks(file, unit = "h")
  }, "could not be properly detected")
})
