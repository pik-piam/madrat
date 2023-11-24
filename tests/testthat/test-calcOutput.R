cfg <- getConfig(verbose = FALSE)

nc <- function(x) {
  getComment(x) <- NULL
  return(x)
}


test_that("calcOutput will stop if unused arguments are provided", {
  calcTest1 <- function(testarg = FALSE) {
    return(list(x = as.magpie(0),
                weight = NULL,
                isocountries = FALSE,
                unit = "1",
                description = "calcOutput test data dummy"))
  }
  globalassign("calcTest1")
  expect_error(co <- capture.output(calcOutput("Test1", testarg = TRUE, blubba = 1, aggregate = FALSE)),
               "unused argument \\(blubba = 1\\)")
})

test_that("Malformed inputs are properly detected", {
  skip_on_cran()
  skip_if_offline("zenodo.org")
  expect_error(localConfig(packages = "nonexistentpackage"),
               'Setting "packages" can only be set to installed packages')
  expect_error(calcOutput("TauTotal", aggregate = "wtf"),
               "None of the columns given in aggregate = wtf could be found in the mappings!")
  expect_error(calcOutput(TRUE), "Invalid type \\(must be a character\\)")
  expect_error(calcOutput(c("a", "b")), "Invalid type \\(must be a single character string\\)")
})

test_that("Malformed calc outputs are properly detected", {
  localConfig(verbosity = 0, .verbose = FALSE)
  calcBla1 <- function() return(as.magpie(1))
  calcBla2 <- function() return(list(x = 1, weight = NULL))
  calcBla3 <- function() return(list(x = as.magpie(1), weight = 1))
  calcBla4 <- function() return(list(x = as.magpie(1), weight = new.magpie(years = 1:2)))
  calcBla5 <- function() {
    return(list(x = new.magpie(years = 1:2, fill = 1),
                weight = new.magpie(years = 3, fill = 1),
                unit = "1",
                description = "test"))
  }
  calcBla6 <- function() {
    return(list(x = new.magpie(years = 1:2, fill = 1),
                weight = new.magpie(years = 3, fill = 1),
                description = "test"))
  }
  calcBla7 <- function() {
    return(list(x = new.magpie(years = 1:2, fill = 1),
                weight = new.magpie(years = 3, fill = 1),
                unit = "1"))
  }
  calcBla8 <- function() {
    return(list(x = new.magpie(years = 1:2),
                weight = new.magpie(years = 3, fill = 1),
                unit = "1",
                description = "test"))
  }
  calcBla9 <- function() {
    return(list(x = new.magpie(years = 1:2, fill = 1),
                weight = new.magpie(years = 3, fill = 1),
                unit = "1",
                description = "test",
                max = 0))
  }
  calcBla10 <- function() {
    return(list(x = new.magpie(years = 1:2, fill = 1),
                weight = new.magpie(years = 3, fill = 1),
                unit = "1",
                description = "test",
                min = 10))
  }
  calcBla11 <- function() return(list(x = 1, class = list))
  calcBla12 <- function() return(list(x = 1, class = c("classA", "classB")))
  calcBla13 <- function() return(list(x = 1, class = "list"))
  calcBla14 <- function() {
    return(list(x = list(1),
                class = "list",
                unit = "1",
                description = "test"))
  }
  globalassign(paste0("calcBla", 1:14))

  expect_error(calcOutput("Bla1"), "not list of two MAgPIE objects")
  expect_error(calcOutput("Bla2"), "Output x of function .* is not a MAgPIE object")
  expect_error(calcOutput("Bla3"), "Output weight of function .* is not a MAgPIE object")
  expect_error(calcOutput("Bla4"), "Number of years disagree between data and weight")
  expect_error(calcOutput("Bla5"), "Neither .* contain a mapping compatible to the provided data")
  expect_warning(calcOutput("Bla6", aggregate = FALSE), "Missing unit information")
  expect_warning(calcOutput("Bla7", aggregate = FALSE), "Missing description")
  expect_warning(calcOutput("Bla8", aggregate = FALSE), "contains NAs")
  expect_warning(calcOutput("Bla9", aggregate = FALSE), "values greater than the predefined maximum")
  expect_warning(calcOutput("Bla10", aggregate = FALSE), "values smaller than the predefined minimum")
  expect_error(calcOutput("Bla11"), "class must be a single element of class character or NULL!")
  expect_error(calcOutput("Bla12"), "class must be a single element of class character or NULL!")
  expect_error(calcOutput("Bla13"), "Output x of function .* is not of promised class")
  expect_error(calcOutput("Bla14"), "Aggregation can only be used in combination with x\\$class=\"magpie\"")

  a <- calcOutput("Bla5", aggregate = FALSE)
  writeLines("CorruptCache", cacheName("calc", "Bla5", packages = "madrat", mode = "get"))
  expect_warning(b <- calcOutput("Bla5", aggregate = FALSE), "corrupt cache")
  expect_identical(nc(a), nc(b))
  expect_identical(nc(b), nc(calcOutput("Bla5", aggregate = FALSE)))

  calcError <- function() stop("I am an error!")
  globalassign("calcError")
  expect_warning(suppressMessages(a <- calcOutput("Error", try = TRUE)), "I am an error", )
  expect_identical(class(a), "try-error")
})

test_that("Calculation for tau example data set works", {
  skip_on_cran()
  skip_if_offline("zenodo.org")
  sink(tempfile())
  require(magclass)
  localConfig(ignorecache = FALSE, forcecache = FALSE, verbosity = 2)
  expectedResult <- new("magpie",
                        .Data = structure(c(0.99, 0.83, 0.68, 1.47, 0.9, 0.64, 0.8, 0.97, 1.17, 0.89, 1.27, 1.25),
                                          .Dim = c(12L, 1L, 1L),
                                          .Dimnames = list(region = c("LAM", "OAS", "SSA", "EUR", "NEU", "MEA",
                                                                      "REF", "CAZ", "CHA", "IND", "JPN", "USA"),
                                                           year = NULL, data = NULL)))
  x <- calcOutput("TauTotal", source = "historical", years = 1995, round = 2, supplementary = TRUE)
  expect_true(is.list(x))
  expect_equivalent(x$x, expectedResult)
  expect_message(x <- readSource("Tau", "historical"), "loading cache")
  expect_error(x <- readSource("Tau", "wtf"), "Unknown subtype")
  expect_warning(calcOutput("TauTotal", source = "historical", years = 1800), "Some years are missing")

  x <- suppressWarnings(calcOutput("TauTotal", source = "historical", years = seq(1970, 2050, 1),
                                   round = 2, supplementary = FALSE))
  expect_true(length(getYears(x, as.integer = TRUE)) == 38)
  expect_true(1970 %in% getYears(x, as.integer = TRUE))
  expect_true(2007 %in% getYears(x, as.integer = TRUE))
  expect_false(2008 %in% getYears(x, as.integer = TRUE))
  sink()
})


test_that("Standard workflow works", {
  downloadTest2 <- function() {
    a <- as.magpie(1)
    getCells(a) <- "DEU"
    write.magpie(a, "test.mz")
  }
  readTest2 <- function() return(read.magpie("test.mz"))
  convertTest2 <- function(x) return(toolCountryFill(x, fill = 10))
  calcTest2 <- function() {
    return(list(x = readSource("Test2"),
                weight = NULL,
                unit = "1"))
  }
  fullTEST2 <- function(rev = 0, dev = "") {
    expectedOutput <- new("magpie",
                          .Data = structure(c(540, 490, 510, 331, 160, 210, 120, 50, 40, 10, 10, 10),
                                            .Dim = c(12L, 1L, 1L),
                                            .Dimnames = list(fake = c("LAM", "OAS", "SSA", "EUR", "NEU", "MEA", "REF",
                                                                      "CAZ", "CHA", "IND", "JPN", "USA"),
                                                             year = NULL, data = NULL)))

    expect_warning(co <- capture.output(a <- calcOutput("Test2", file = "test.mz")),
                   'Missing description for data set "Test2"')
    expect_equivalent(a, expectedOutput)
  }
  globalassign("downloadTest2", "readTest2", "convertTest2", "calcTest2", "fullTEST2")
  co <- capture.output(retrieveData("test2", puc = FALSE))
})

test_that("Custom class support works", {
  localConfig(outputfolder = withr::local_tempdir(), verbosity = 0, .verbose = FALSE)
  calcBla1 <- function() {
    return(list(x          = list(1),
                class      = "list",
                unit       = "1",
                description = "test"))
  }
  globalassign(paste0("calcBla", 1))
  data <- calcOutput("Bla1", aggregate = FALSE, file = "test.rds")
  expect_equivalent(data, list(1))
  expect_identical(readRDS(file.path(getConfig("outputfolder"), "test.rds")), data)
})

test_that("Old descriptors are properly removed from comment", {
  localConfig(outputfolder = withr::local_tempdir(), verbosity = 0, .verbose = FALSE)
  calcBlub <- function() {
    x <- as.magpie(1)
    getComment(x) <- "test comment"
    return(list(x           = x,
                unit        = "1",
                description = "Descriptor test ",
                title       = "Blub"))
  }

  calcBlub2 <- function() {
    return(list(x           = calcOutput("Blub", aggregate = FALSE),
                unit        = "1",
                description = "Descriptor test 2",
                title       = "Blub2"))
  }

  globalassign("calcBlub", "calcBlub2")
  a <- calcOutput("Blub", aggregate = FALSE)
  expect_true(" comment: test comment" %in% getComment(a))
  a <- calcOutput("Blub2", aggregate = FALSE)
  expect_false(any(grepl("comment:", getComment(a))))
})

test_that("Aggregation works", {
  localConfig(outputfolder = withr::local_tempdir(), verbosity = 0, .verbose = FALSE)
  calcAggregationTest <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data",
                unit = "1"))
  }
  calcAggregationTest2 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data 2",
                unit = "1",
                mixed_aggregation = TRUE,
                aggregationFunction = function(x, rel, mixed_aggregation) return(as.magpie(1)))) # nolint
  }
  calcAggregationTest3 <- function() {
    x1 <- new.magpie(getISOlist(), fill = 1)
    getSets(x1)[1] <- "country"
    x2 <- new.magpie(1:4, fill = 2)
    x <- x1 * x2
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data 3",
                unit = "1"))
  }
  calcAggregationTest4 <- function() {
    x1 <- new.magpie(getISOlist()[1:12], fill = 1)
    getSets(x1)[1] <- "country"
    x2 <- new.magpie(1:4, fill = 2)
    x <- x1 * x2
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data 3",
                unit = "1",
                isocountries = FALSE))
  }
  calcMalformedAggregation <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data 2",
                unit = "1",
                mixed_aggregation = TRUE,
                aggregationFunction = 99))
  }
  calcMalformedAggregation2 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data 2",
                unit = "1",
                mixed_aggregation = TRUE,
                aggregationArguments = 42,
                aggregationFunction = function(x, rel, mixed_aggregation) return(as.magpie(1)))) # nolint
  }
  globalassign("calcAggregationTest", "calcAggregationTest2", "calcAggregationTest3", "calcAggregationTest4",
               "calcMalformedAggregation", "calcMalformedAggregation2")

  reg <- new("magpie", .Data = structure(c(54, 49, 51, 34, 16, 21, 12,
                                           5, 4, 1, 1, 1), .Dim = c(12L, 1L, 1L),
                                         .Dimnames = list(region = c("LAM", "OAS", "SSA", "EUR", "NEU",
                                                                     "MEA", "REF", "CAZ", "CHA", "IND",
                                                                     "JPN", "USA"),
                                                          year = NULL, data = NULL)))
  glo <- new("magpie", .Data = structure(249, .Dim = c(1L, 1L, 1L),
                                         .Dimnames = list(region = "GLO", year = NULL, data = NULL)))

  country2 <- new("magpie", .Data = structure(rep(8, 249), .Dim = c(249L, 1L, 1L),
                                              .Dimnames = list(country = unname(getISOlist()),
                                                               year = NULL, data = NULL)))

  reg2 <- new("magpie", .Data = structure(c(432, 392, 408, 272, 128, 168,
                                            96, 40, 32, 8, 8, 8), .Dim = c(12L, 1L, 1L),
                                          .Dimnames = list(region = c("LAM", "OAS", "SSA", "EUR", "NEU", "MEA",
                                                                      "REF", "CAZ", "CHA", "IND", "JPN", "USA"),
                                                           year = NULL, data = NULL)))
  glo2 <- new("magpie", .Data = structure(1992, .Dim = c(1L, 1L, 1L),
                                          .Dimnames = list(global = "GLO", year = NULL, data = NULL)))

  region1 <- new("magpie", .Data = structure(rep(24, 4), .Dim = c(4L, 1L, 1L),
                                             .Dimnames = list(region1 = c("1", "2", "3", "4"),
                                                              year = NULL, data = NULL)))

  expect_identical(nc(calcOutput("AggregationTest")), reg)
  expect_identical(nc(calcOutput("AggregationTest2")), clean_magpie(as.magpie(1)))
  expect_identical(nc(calcOutput("AggregationTest3")), reg2)
  expect_identical(nc(calcOutput("AggregationTest", aggregate = "glo")), glo)
  expect_identical(nc(calcOutput("AggregationTest3", aggregate = "glo")), glo2)
  expect_identical(nc(calcOutput("AggregationTest3", aggregate = "country")), country2)
  expect_error(calcOutput("AggregationTest4", aggregate = TRUE), "Cannot aggregate to regions")
  expect_identical(nc(calcOutput("AggregationTest4", aggregate = "region1")), region1)
  expect_identical(nc(calcOutput("AggregationTest", aggregate = "regglo")), mbind(reg, glo))
  expect_warning(a <- nc(calcOutput("AggregationTest", aggregate = "global+region+cheese")),
                 "Omitting cheese from aggregate")
  expect_identical(a, mbind(glo, reg))
  expect_error(calcOutput("MalformedAggregation"), "must be a function")
  expect_error(calcOutput("MalformedAggregation2"), "must be a list of function arguments")

  xtramap <- file.path(withr::local_tempdir(), "blub.csv")
  file.copy(toolGetMapping(getConfig("regionmapping"), returnPathOnly = TRUE), xtramap)
  localConfig(extramappings = xtramap)

  # use 'local' to have the change of verbosity level only local and let the remainder of the script unaffected
  local({
    # set verbosity to a level that will produce the expected NOTE
    localConfig(verbosity = 1)
    expect_message(a <- nc(calcOutput("AggregationTest", aggregate = "glo")),
                   paste0("Ignoring column\\(s\\) X, region, global from .* as the column\\(s\\) ",
                          "already exist in another mapping\\."))
    expect_identical(a, glo)
  })
})

test_that("1on1 country mappings do not alter the data", {
  map <- data.frame(country = getISOlist(), region = getISOlist())
  tmpFile <- withr::local_tempfile(fileext = ".csv")
  write.csv(map, tmpFile)
  localConfig(outputfolder = withr::local_tempdir(),
              regionmapping = tmpFile,
              verbosity = 0, .verbose = FALSE)

  expect_equal(nc(calcOutput("TauTotal")), nc(calcOutput("TauTotal", aggregate = FALSE)))


  calc1on1Test <- function() {
    x1 <- new.magpie(getISOlist(), fill = 1)
    getSets(x1)[1] <- "country"
    x2 <- new.magpie(1:4, fill = 2)
    x <- x1 * x2
    # fill with random numbers and mix order to test whether this
    # affects the country-country mapping
    x[, , ] <- unlist(randu)[seq_len(length(x))]
    x <- x[order(x), , ]
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data 3",
                unit = "1"))
  }
  globalassign("calc1on1Test")
  aCountry <- magpiesort(nc(calcOutput("1on1Test", aggregate = "country")))
  getSets(aCountry)[1] <- "region"
  aRegion <- magpiesort(nc(calcOutput("1on1Test")))
  expect_equal(aRegion, aCountry)
})

test_that("Bilateral aggregation works", {
  calcBilateral <- function() {
    map <- toolGetMapping("regionmappingH12.csv", where = "madrat")
    tmp <- expand.grid(map[[2]], map[[2]], stringsAsFactors = FALSE)
    x <- new.magpie(paste0(tmp[[1]], ".", tmp[[2]]))
    x[, , ] <- rep(seq_len(nrow(map)), nrow(map))
    return(list(x = x, weight = x, unit = "SpaceDollar", description = "Test data set"))
  }
  localConfig(verbosity = 0, .verbose = FALSE)
  globalassign("calcBilateral")
  aExp <- new("magpie",
              .Data = structure(c(161.215558601782, 176.61906116643, 171.777380952381),
                                .Dim = c(3L, 1L, 1L),
                                .Dimnames = list(region.region1 = c("LAM.LAM", "OAS.LAM", "SSA.LAM"),
                                                 year = NULL, data = NULL)))

  a <- calcOutput("Bilateral")
  getComment(a) <- NULL
  expect_equal(head(a), aExp)
})

test_that("Edge cases work as expected", {
  localConfig(outputfolder = withr::local_tempdir(), verbosity = 0, .verbose = FALSE)
  calcEdgeTest <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data",
                unit = "1"))
  }
  calcEdgeTest2 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    getYears(x) <- 2000
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data",
                unit = "1"))
  }
  calcNoMag <- function() {
    x <- list(1, 2, 3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description = "Non magclass test",
                unit = "1"))
  }
  calcNoMag2 <- function() {
    x <- list(1, 2, 3)
    return(list(x = x,
                weight = 12,
                class = "list",
                description = "Non magclass test",
                unit = "1"))
  }
  calcNoMag3 <- function() {
    x <- list(1, 2, 3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description = "Non magclass test",
                min = 0,
                unit = "1"))
  }
  calcNoMag4 <- function() {
    x <- list(1, 2, 3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description = "Non magclass test",
                structure.data = list(),
                unit = "1"))
  }
  calcNoMag5 <- function() {
    x <- list(1, 2, 3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description = "Non magclass test",
                isocountries = TRUE,
                unit = "1"))
  }
  globalassign("calcEdgeTest", "calcEdgeTest2",
               "calcNoMag", "calcNoMag2", "calcNoMag3", "calcNoMag4", "calcNoMag5")
  expect_warning(calcOutput("EdgeTest", append = TRUE), "works only when the file name is provided")
  expect_warning(calcOutput("EdgeTest", file = "blub.mif"),
                 "Time dimension missing and data cannot be written to a mif-file")
  a <- calcOutput("EdgeTest2", file = "blub.rds")
  expect_identical(a, readRDS(paste0(getConfig("outputfolder"), "/blub.rds")))

  expect_error(calcOutput("NoMag"), "Aggregation can only be used")
  expect_identical(nc(calcOutput("NoMag", aggregate = FALSE)), list(1, 2, 3))
  # now from cache...
  expect_identical(nc(calcOutput("NoMag", aggregate = FALSE)), list(1, 2, 3))
  expect_error(calcOutput("NoMag", aggregate = FALSE, round = 0), "rounding can only be used")
  expect_error(calcOutput("NoMag", aggregate = FALSE, years = 2000), "years argument can only be used")
  expect_error(calcOutput("NoMag", aggregate = FALSE, file = "bla.mz"), "Unsupported file format")
  expect_error(calcOutput("NoMag2", aggregate = FALSE), "Weights are currently not supported")
  expect_error(calcOutput("NoMag3", aggregate = FALSE), "Min/Max checks cannot be used")
  expect_error(calcOutput("NoMag4", aggregate = FALSE), "Structure checks cannot be used")
  expect_error(calcOutput("NoMag5", aggregate = FALSE), "isocountries can only be set if")

  skip_if_not_installed("reshape2")
  a <- calcOutput("EdgeTest2", file = "blub.mif")
  b <- magclass::read.report(file.path(getConfig("outputfolder"), "blub.mif"), as.list = FALSE)
  expect_identical(sum(a - b), 0)
})

test_that("Data check works as expected", {
  localConfig(outputfolder = withr::local_tempdir(), verbosity = 0, .verbose = FALSE)
  calcMalformedISO <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                isocountries = 12,
                unit = "1"))
  }
  calcMalformedMixed <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                mixed_aggregation = 12,
                unit = "1"))
  }
  calcMalformedISO2 <- function() {
    x <- as.magpie(1)
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                isocountries = TRUE,
                unit = "1"))
  }
  calcMalformedISO3 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    getCells(x)[1] <- "BLA"
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                isocountries = TRUE,
                unit = "1"))
  }
  calcMalformedStruct <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                structure.spatial = "ABC",
                unit = "1"))
  }
  calcMalformedStruct2 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                structure.temporal = "y[0-9]*",
                unit = "1"))
  }
  calcMatchingStruct <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                structure.spatial = "[A-Z]{3}",
                unit = "1"))
  }
  calcInfinite <- function() {
    x <- as.magpie(Inf)
    return(list(x = x,
                weight = NULL,
                description = "Malformed example",
                unit = "1"))
  }
  globalassign("calcMalformedISO", "calcMalformedMixed",
               "calcMalformedISO2", "calcMalformedISO3",
               "calcMalformedStruct", "calcMalformedStruct2",
               "calcMatchingStruct", "calcInfinite")
  expect_error(calcOutput("MalformedISO"), "isocountries must be a logical")
  expect_error(calcOutput("MalformedMixed"), "mixed_aggregation must be a logical")
  expect_error(calcOutput("MalformedISO2"), "Wrong number of countries")
  expect_error(calcOutput("MalformedISO3"), "Countries .* do not agree with iso country list")
  expect_warning(calcOutput("MalformedStruct"), "Invalid names")
  expect_warning(calcOutput("MalformedStruct2"), "Missing names")
  expect_silent(suppressMessages(calcOutput("MatchingStruct")))
  cache <- cacheName("calc", "MatchingStruct")
  a <- readRDS(cache)
  getCells(a$x)[1] <- "BLA"
  saveRDS(a, cache)
  localConfig(verbosity = 2, .verbose = FALSE)
  expect_message(calcOutput("MatchingStruct"), "cache file corrupt")
  expect_warning(calcOutput("Infinite", aggregate = FALSE), "infinite values")
})
