test_that("toolCountryFill works as expected", {
  x <- new.magpie("DEU", 1994, "bla", 0)
  expect_message(y <- toolCountryFill(x, 99), "toolCountryFill set missing values for IMPORTANT")
  expect_equal(dim(y)[1], 249)
  expect_true(all(y["DEU", , ] == 0))
  expect_true(all(y["DEU", , , invert = TRUE] == 99))

  x2 <- new.magpie(c("DEU", "XYZ"), 1994, "bla", 0)
  expect_warning(y2 <- toolCountryFill(x2, 99), "unknown country codes removed: XYZ")
  expect_identical(y2, y)

  expect_error(toolCountryFill(x, 99, HKG = "CHN"), "Try to fill a country")
  expect_silent(suppressMessages({
    y3 <- toolCountryFill(x, 99, FRA = "DEU", verbosity = 2)
  }))
  expect_true(all(y3[c("DEU", "FRA"), , ] == 0))
  y4 <- toolCountryFill(x, 99, map = c(FRA = "DEU"), verbosity = 2)
  expect_identical(y3, y4)

  x3 <- new.magpie(c("DEU", "FRA"), 1994, "bla", 0)
  x3["FRA", , ] <- 42
  expect_true(all(toolCountryFill(x3, 99, FRA = "DEU")["FRA", , ] == 42))
  expect_message(y5 <- toolCountryFill(x3, 99, FRA = "DEU", overwrite = TRUE), "data overwritten")
  expect_true(all(y5["FRA", , ] == 0))

  empty <- new.magpie(NULL, NULL, c("bla"))
  expect_silent(y6 <- toolCountryFill(empty, verbosity = 3))
  expect_identical(getItems(y6, dim = c(2, 3)), getItems(empty, dim = c(2, 3)))
  expect_identical(getItems(y6, dim = 1), getItems(y5, dim = 1))

})
