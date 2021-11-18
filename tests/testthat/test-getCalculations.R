test_that("getCalculations works", {
  setConfig(globalenv = FALSE, .verbose = FALSE, .local = TRUE)
  expect_warning({
    x <- getCalculations(packages = "blub")
  }, "is not available")
  expect_null(x)

  xe <- structure(list(type = c("TauTotal", "Tau"), package = c("madrat", "madrat"),
                       call = c("madrat:::calcTauTotal", "madrat:::readTau")),
                  row.names = c(1L, 2L), class = "data.frame")
  expect_identical(getCalculations(c("calc", "read"), packages = "madrat"), xe)
})
