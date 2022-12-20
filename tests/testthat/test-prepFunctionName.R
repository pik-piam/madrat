test_that("prepFunctionName works", {
  expect_error(madrat:::prepFunctionName("Missing"), "not a valid output type")
  expect_null(madrat:::prepFunctionName("Missing", error_on_missing = FALSE))
  expect_identical(as.character(madrat:::prepFunctionName("TauTotal")), "madrat:::calcTauTotal(...)")
  calcTauTotal <- function() return(1)
  globalassign("calcTauTotal")
  expect_error(suppressWarnings(madrat:::prepFunctionName("TauTotal")), "Cannot substitute")
})
