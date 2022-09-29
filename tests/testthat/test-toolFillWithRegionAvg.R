p <- magclass::maxample("pop")

test_that("Fill with regional average works as expected", {
  x <- magclass::new.magpie(cells_and_regions = c("A", "B", "C", "D"), years = c(2000, 2005),
                            fill = c(1, NA, 3, 4, 5, 6, NA, 8))
  rel <- data.frame(CountryCode = c("A", "B", "C", "D"), RegionCode = c("R1", "R1", "R1", "R2"))
  expect_silent(xfilled <- toolFillWithRegionAvg(x, regionmapping = rel, verbose = FALSE))
  expect_equal(as.vector(xfilled["B", 2000, 1]), 2)
  expect_equal(as.vector(xfilled["C", 2005, 1]), 11 / 2)
  expect_true(all(!is.na(xfilled)))
  ref <- new("magpie", .Data = structure(c(1, 2.5, 3, 4, 5, 6, 5.55, 8),
             .Dim = c(4L, 2L, 1L), .Dimnames = list(region = c("A", "B", "C", "D"),
                                            year = c("y2000", "y2005"), data = NULL)))
  expect_identical(round(toolFillWithRegionAvg(x, regionmapping = rel, verbose = FALSE, weight = x), 2), ref)

  x <- new.magpie(c("DEU", "FRA"))
  x[] <- 1:2
  expect_message({
    y <- toolFillWithRegionAvg(x, valueToReplace = 1)
  }, "Replaced missing values")
  expect_identical(as.vector(y), c(2, 2))
  expect_warning({
    y <- toolFillWithRegionAvg(x, valueToReplace = 1, callToolCountryFill = TRUE)
  }, "More than .* missing")
  expect_equal(dim(y)[1], 249)
})

test_that("Malformed inputs are properly detected", {
  expect_error(toolFillWithRegionAvg(1), "has to be a MAgPIE object")
  expect_error(toolFillWithRegionAvg(p), "Only one element")
  expect_error(toolFillWithRegionAvg(p[, , 1], weight = p), "must have exactly one")
  expect_error(toolFillWithRegionAvg(p[, , 1], weight = p[2:5, , 1]), "Regions .* do not match")
  expect_error(toolFillWithRegionAvg(p[, , 1], weight = p[, 2:3, 1]), "Years .* do not match")
})
