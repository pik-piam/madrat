context("Test toolSubtypeSelect")

test_that("toolSubtypeSelect works as expected", {
  expect_identical(toolSubtypeSelect("blub", c(bla = 12, blub = "hallo")), "hallo")
  expect_error(toolSubtypeSelect("abc", c(bla = 12, blub = "hallo")), "Unknown subtype")
  expect_identical(toolSubtypeSelect("blub", list(bla = 12, blub = 1:10)), 1:10)
})
