test_that("getCalculations works", {
  expect_identical(getLocation("TauTotal", packages = "madrat"), "madrat")
  expect_identical(getLocation("calcTauTotal", packages = "madrat"), "madrat")
  expect_null(getLocation("readTauTotal", packages = "madrat"))
  calcLocationTest <- function() return(NULL)
  globalassign("calcTauTotal")
  expect_identical(getLocation("TauTotal", packages = "madrat"), c("madrat", ".GlobalEnv"))
  expect_identical(getLocation("TauTotal", packages = "madrat", globalenv = FALSE), "madrat")
})
