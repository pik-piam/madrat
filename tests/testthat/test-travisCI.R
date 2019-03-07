library(testthat)
skip_if_not(identical(Sys.getenv("TRAVIS"), "true"))
context("Travis CI integration")

test_that("Validation Key is correct (for Travis CI)", {
  expect_true(lucode:::validkey()$valid)
})