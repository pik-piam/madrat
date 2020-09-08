context("toolGetMapping")

test_that("toolGetMapping works", {
  expected <- structure(list(X = c("Aruba", "Afghanistan", "Angola", "Anguilla",
                                   "Aland Islands", "Albania"), 
                             CountryCode = c("ABW", "AFG", "AGO","AIA", "ALA", "ALB"), 
                             RegionCode = c("LAM", "OAS", "SSA", "LAM","EUR", "NEU")), 
                        row.names = c(NA, 6L), class = "data.frame")
  expect_identical(head(toolGetMapping("regionmappingH12.csv", where="madrat")), expected)
  expect_identical(toolGetMapping("bla",where="local", returnPathOnly = TRUE), "bla")
  expect_identical(toolGetMapping("/bla",where="local", returnPathOnly = TRUE), "/bla")
  expect_identical(toolGetMapping("/bla",type=".",where="local", returnPathOnly = TRUE), "./bla")
})
