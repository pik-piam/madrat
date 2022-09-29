test_that("toolFillYears works", {
  p <- magclass::maxample("pop")[1, , 1]
  expected <- new("magpie",
                  .Data = structure(c(696, 696, 735, 774, 812, 851,
                                      889, 889), .Dim = c(1L, 8L, 1L),
                                    .Dimnames = list(i = "AFR",
                                                     t = c("y2003", "y2005", "y2007",
                                                           "y2009", "y2011", "y2013",
                                                           "y2015", "y2017"),
                                                     scenario = "A2")))
  expect_identical(round(toolFillYears(p, paste0("y", seq(2003, 2017, 2)))), expected)
})

test_that("toolXlargest works", {
  expect_error(toolXlargest(1), "must be a MAgPIE object")
  expect_identical(toolXlargest(magclass::maxample("pop"), range = 1:3, years = c(1995, 2005),
                                elements = "A2"), c("SAS", "CPA", "AFR"))
})

test_that("toolSubtypeSelect works as expected", {
  expect_identical(toolSubtypeSelect("blub", c(bla = 12, blub = "hallo")), "hallo")
  expect_error(toolSubtypeSelect(NULL, c(bla = 12, blub = "hallo")), "Subtype has to be set")
  expect_error(toolSubtypeSelect("abc", c(bla = 12, blub = "hallo")), "Unknown subtype")
  expect_identical(toolSubtypeSelect("blub", list(bla = 12, blub = 1:10)), 1:10)
})

test_that("toolNAreplace works as expected", {
  a <- magclass::maxample("animal")
  p <- magclass::maxample("pop")[1:3, 1:2, ]
  attr(p, "Metadata") <- NULL
  p2 <- p
  p[seq(1, 11, 2)]  <- NA
  p2[seq(1, 11, 2)] <- 99
  expect_identical(toolNAreplace(p, replaceby = 99)$x, p2)
  expect_identical(toolNAreplace(p, replaceby = as.magpie(99))$x, p2)
  expect_error(toolNAreplace(p, replaceby = a), "replaceby must be expandable")
  expect_error(toolNAreplace(p, replaceby = 1:10), "length different than 1")

  p[, , ] <- 1:12
  r <- p
  r[, , ] <- c(1:4, rep(-1, 6), 11, 12)
  expect_identical(toolNAreplace(p, replaceby = -1, val.rm = 5:10)$x, r)
  w <- p
  w[, , ] <- 1
  r[, , ] <- c(rep(1, 4), rep(0, 6), 1, 1)
  expect_identical(toolNAreplace(p, weight = w, replaceby = -1, val.rm = 5:10)$weight, r)
})

test_that("toolTimeSpline works as expected", {
  expect_error(toolTimeSpline(1), "not a MAgPIE object")

  p <- magclass::maxample("pop")[1:2, 1:6, 1]
  attr(p, "Metadata") <- NULL

  o5 <- new("magpie", .Data = structure(c(513.1, 1314.4, 711.9, 1411.1, 920.9, 1499.2, 1146.2,
                                          1574.7, 1386, 1638.5, 1633.3, 1695.7),
                                        .Dim = c(2L, 6L, 1L),
                                        .Dimnames = list(i = c("AFR", "CPA"),
                                                         t = c("y1995", "y2005", "y2015", "y2025", "y2035", "y2045"),
                                                         scenario = "A2")))
  o3 <- new("magpie", .Data = structure(c(491.3, 1332, 715.5, 1408.1, 939.8, 1484.2, 1164, 1560.3,
                                          1388.3, 1636.5, 1612.5, 1712.6),
                                        .Dim = c(2L, 6L, 1L),
                                        .Dimnames = list(i = c("AFR", "CPA"),
                                                         t = c("y1995", "y2005", "y2015", "y2025", "y2035", "y2045"),
                                                         scenario = "A2")))
  ncr <- function(x) {
    getComment(x) <- NULL
    return(round(x, 1))
  }

  expect_lt(max(abs(ncr(toolTimeSpline(p)) - o5)), 0.3)
  expect_lt(max(abs(ncr(toolTimeSpline(p, dof = 5)) - o5)), 0.3)
  expect_lt(max(abs(ncr(toolTimeSpline(p, dof = 3)) - o3)), 0.3)

  expect_warning({
    p5 <- ncr(toolTimeSpline(p, dof = 0))
  }, "dof values < 1 not allowed!")
  expect_lt(max(abs(p5 - o5)), 0.3)
  expect_warning({
    p100 <- ncr(toolTimeSpline(p, dof = 100))
  }, "Degrees of freedom too high")
  expect_identical(p100, ncr(p))
})

test_that("toolConvertMapping works as expected", {
  localConfig(mappingfolder = withr::local_tempdir(), .verbose = FALSE)
  file.copy(toolGetMapping("regionmappingH12.csv", returnPathOnly = TRUE), getConfig("mappingfolder"))

  expect_silent(toolConvertMapping("regionmappingH12.csv"))
  expect_true(file.exists(toolGetMapping("regionmappingH12.rds", returnPathOnly = TRUE)))
  expect_identical(toolGetMapping("regionmappingH12.rds"), toolGetMapping("regionmappingH12.csv"))

  expect_silent(toolConvertMapping("regionmappingH12.csv", format = "rda"))
  expect_true(file.exists(toolGetMapping("regionmappingH12.rda", returnPathOnly = TRUE)))
  expect_identical(toolGetMapping("regionmappingH12.rda"), toolGetMapping("regionmappingH12.csv"))

  unlink(toolGetMapping("regionmappingH12.csv", where = "mappingfolder", returnPathOnly = TRUE))
  expect_error(toolGetMapping("regionmappingH12.csv", where = "mappingfolder", returnPathOnly = TRUE))
  expect_silent(toolConvertMapping("regionmappingH12.rds", format = "csv"))
  expect_true(file.exists(toolGetMapping("regionmappingH12.csv", returnPathOnly = TRUE)))
  expect_identical(toolGetMapping("regionmappingH12.rds"), toolGetMapping("regionmappingH12.csv"))

  expect_error(toolConvertMapping("regionmappingH12.csv", format = "xyz"), "Unsupported format")
})

test_that("toolConditionalReplace works as expected", {

  localConfig(verbosity = 0, .verbose = FALSE)
  m <- as.magpie(c(1, NA, 0, -1, Inf))

  expect_error(toolConditionalReplace(m), "missing, with no default")

  expect_identical(toolConditionalReplace(m, "< -100"), m)
  expect_identical(toolConditionalReplace(m, "is.na()"), as.magpie(c(1, 0, 0, -1, Inf)))
  expect_identical(toolConditionalReplace(m, c("is.na()", "<0"), replaceby = 1:2), as.magpie(c(1, 1, 0, 2, Inf)))
  expect_identical(toolConditionalReplace(m, c("<0", "is.na()"), replaceby = 1:2), as.magpie(c(1, 2, 0, 1, Inf)))
  expect_identical(toolConditionalReplace(m, c("<0", "is.na()"), replaceby = 3), as.magpie(c(1, 3, 0, 3, Inf)))
  expect_error(toolConditionalReplace(m, c("<0", "is.na()"), replaceby = 1:3),
               "has to be of length 1 or the same length")
})

test_that("mad(l)apply function return defunct message", {
  expect_error(madapply(), "defunct")
  expect_error(madlapply(), "defunct")
})

test_that("madrat attach/detach work", {
  expect_silent(madratAttach("mrfancypackage"))
  expect_true("mrfancypackage" %in% getConfig("packages"))
  expect_silent(madratDetach("mrfancypackage"))
  expect_false("mrfancypackage" %in% getConfig("packages"))
})
