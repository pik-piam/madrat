context("Test various tool functions")

test_that("toolFillYears works", {
  p <- maxample("pop")[1,,1]
  expected <- new("magpie", 
                  .Data = structure(c(696, 696, 735, 774, 812, 851,
                                      889, 889), .Dim = c(1L, 8L, 1L),
                                    .Dimnames = list(i = "AFR",
                                                     t = c("y2003", "y2005", "y2007", 
                                                           "y2009", "y2011", "y2013",
                                                           "y2015", "y2017"), 
                                                     scenario = "A2")))
  expect_identical(round(toolFillYears(p, paste0("y",seq(2003,2017,2)))), expected)
})

test_that("toolXlargest works", {
  expect_identical(toolXlargest(maxample("pop"), range=1:3, years=c(1995,2005), elements="A2"), c("SAS","CPA","AFR"))
})