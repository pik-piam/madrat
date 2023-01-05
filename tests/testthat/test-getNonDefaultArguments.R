test_that("getNonDefaultArguments returns only non-default arguments", {
  testFunction <- function(a = 0, b = 0, ...) {
  }

  expect_length(getNonDefaultArguments(testFunction), 0)
  expect_length(getNonDefaultArguments(testFunction, args = list(a = 0)), 0)
  expect_identical(getNonDefaultArguments(testFunction, args = list(a = 1)), list(a = 1))
  expect_identical(getNonDefaultArguments(testFunction, args = list(a = 0, b = 1, x = 1)), list(b = 1, x = 1))
  expect_identical(getNonDefaultArguments(testFunction, args = list(a = 0, x = 1, b = 1)), list(x = 1, b = 1))
})
