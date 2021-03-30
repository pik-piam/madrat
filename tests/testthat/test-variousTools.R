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
  expect_error(toolXlargest(1), "must be a MAgPIE object")
  expect_identical(toolXlargest(maxample("pop"), range=1:3, years=c(1995,2005), elements="A2"), c("SAS","CPA","AFR"))
})

test_that("toolSubtypeSelect works as expected", {
  expect_identical(toolSubtypeSelect("blub", c(bla = 12, blub = "hallo")), "hallo")
  expect_error(toolSubtypeSelect(NULL, c(bla = 12, blub = "hallo")), "Subtype has to be set")
  expect_error(toolSubtypeSelect("abc", c(bla = 12, blub = "hallo")), "Unknown subtype")
  expect_identical(toolSubtypeSelect("blub", list(bla = 12, blub = 1:10)), 1:10)
})

test_that("toolNAreplace works as expected", {
  p <- maxample("pop")[1:3,1:2,]
  attr(p, "Metadata") <- NULL
  p2 <- p
  p[seq(1,11,2)]  <- NA
  p2[seq(1,11,2)] <- 99
  expect_identical(toolNAreplace(p, replaceby = 99)$x, p2)
  expect_identical(toolNAreplace(p, replaceby = as.magpie(99))$x, p2)
})
