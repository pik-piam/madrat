test_that("prepFunctionName works", {
  expect_error(prepFunctionName("Missing"), "not a valid output type")
  expect_null(prepFunctionName("Missing", error_on_missing = FALSE))
  expect_identical(as.character(prepFunctionName("TauTotal")), "madrat:::calcTauTotal(...)")
  calcTauTotal <- function() return(1)
  globalassign("calcTauTotal")
  expect_error(suppressWarnings(prepFunctionName("TauTotal")), "Cannot substitute")
})
