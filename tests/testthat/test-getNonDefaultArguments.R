test_that("getNonDefaultArguments returns only non-default arguments", {
  testFunction <- function(a = 0, b = 0, ...) {
  }

  expect_length(getNonDefaultArguments(testFunction, args = NULL), 0)
  expect_length(getNonDefaultArguments(testFunction, args = list(a = 0)), 0)
  expect_identical(getNonDefaultArguments(testFunction, args = list(a = 1)), list(a = 1))
  expect_identical(getNonDefaultArguments(testFunction, args = list(a = 0, b = 1, x = 1)), list(b = 1, x = 1))
  expect_identical(getNonDefaultArguments(testFunction, args = list(a = 0, x = 1, b = 1)), list(x = 1, b = 1))
})

test_that("getNonDefaultArguments can handle read + convert with subtype = NULL", {
  readTest <- function(subtype = NULL) {}
  convertTest <- function(x) {}
  globalassign("readTest", "convertTest")

  expect_length(getNonDefaultArguments(c("readTest", "convertTest"),
                                       list(subtype = "a", x = 5)), 2)
})

test_that("getNonDefaultArguments checks argument names, not values", {
  testNamesFunction <- function(a, b = NULL, c = NULL) {}
  globalassign("testNamesFunction")

  # identical (large) values under accepted names must not be flagged
  x <- magclass::new.magpie(paste0("R", 1:200), 1965:2020, paste0("d", 1:50), fill = NaN)
  expect_silent(getNonDefaultArguments("testNamesFunction", args = list(a = x, b = x, c = NULL)))

  # an unknown name must be flagged even if its value equals an accepted argument's value
  expect_error(getNonDefaultArguments("testNamesFunction", args = list(a = 1, z = 1)),
               "unexpected arguments were passed to .*: z\n")
  expect_error(getNonDefaultArguments("testNamesFunction", args = list(a = x, z = x)),
               "unexpected arguments were passed to .*: z\n")
})
