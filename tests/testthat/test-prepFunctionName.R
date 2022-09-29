globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
}

test_that("prepFunctionName works", {
  expect_error(madrat:::prepFunctionName("Missing"), "not a valid output type")
  expect_null(madrat:::prepFunctionName("Missing", error_on_missing = FALSE))
  expect_identical(as.character(madrat:::prepFunctionName("TauTotal")), "madrat:::calcTauTotal(...)")
  calcTauTotal <- function() return(1)
  globalassign("calcTauTotal")
  localConfig(globalenv = TRUE, verbosity = FALSE)
  expect_error(suppressWarnings(madrat:::prepFunctionName("TauTotal")), "Cannot substitute")
  rm(list = "calcTauTotal", envir = .GlobalEnv)
})

rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
