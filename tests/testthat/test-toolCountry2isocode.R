context("toolCountry2isocode")

test_that("Country-to-ISO mapping works", {
  expect_equal(toolCountry2isocode("Germany"),"DEU")
  expect_equal(toolCountry2isocode(c("France","Fantasyland"),mapping=c("Fantasyland"="BLA")),
               c("FRA","BLA"))
})
