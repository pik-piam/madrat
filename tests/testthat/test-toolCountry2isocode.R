test_that("Country-to-ISO mapping works", {
  expect_equal(toolCountry2isocode("Germany"), "DEU")
  expect_equal(toolCountry2isocode(c("France", "Fantasyland"), mapping = c("Fantasyland" = "BLA")),
               c("FRA", "BLA"))
  expect_warning(toolCountry2isocode(c("Germany", "Fantasyland")), "Following country names could not be found")
  expect_silent({
    a <- toolCountry2isocode(c("Germany", "Fantasyland"), warn = FALSE)
  })
  expect_equal(a, c("DEU", NA))
  expect_warning(toolCountry2isocode("Germany", type = "bla"), "deprecated")

})
