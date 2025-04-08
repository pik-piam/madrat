# SPDX-FileCopyrightText: 2025 Potsdam Institute for Climate Impact Research (PIK)
# SPDX-License-Identifier: BSD-2-Clause

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
