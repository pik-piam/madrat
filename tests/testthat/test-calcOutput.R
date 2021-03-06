context("Data calculation wrapper")

Sys.setenv("LANGUAGE" = "EN")

cfg <- getConfig(verbose = FALSE)

globalassign <- function(...) {
  for (x in c(...)) assign(x,eval.parent(parse(text = x)),.GlobalEnv)
}

nc <- function(x) {
  getComment(x) <- NULL
  return(x)
}


test_that("calcOutput will stop if unused arguments are provided", {
  setConfig(globalenv = TRUE, .verbose = FALSE)
  calcTest1 <- function(testarg=FALSE) {
    return(list(x=as.magpie(0),
                weight=NULL,
                isocountries=FALSE,
                unit="1",
                description="calcOutput test data dummy"))
  }
  globalassign("calcTest1")
  expect_error(co <- capture.output(calcOutput("Test1",testarg=TRUE,blubba=1,aggregate = FALSE)),
               "unused argument \\(blubba = 1\\)")
  setConfig(globalenv = cfg$globalenv, .verbose = FALSE)
})

test_that("Malformed inputs are properly detected", {
  skip_if_offline()
  expect_error(setConfig(packages="nonexistentpackage"),'Setting "packages" can only be set to installed packages')
  expect_error(calcOutput("TauTotal",aggregate = "wtf"), "None of the columns given in aggregate = wtf could be found in the mappings!")
  expect_error(calcOutput(TRUE), "Invalid type \\(must be a character\\)")
  expect_error(calcOutput(c("a","b")), "Invalid type \\(must be a single character string\\)")
})

test_that("Malformed calc outputs are properly detected", {
  setConfig(globalenv = TRUE, verbosity = 0, .verbose = FALSE)
  calcBla1 <- function()return(as.magpie(1))
  calcBla2 <- function()return(list(x=1,weight=NULL))
  calcBla3 <- function()return(list(x=as.magpie(1),weight=1))
  calcBla4 <- function()return(list(x=as.magpie(1),weight=new.magpie(years=1:2)))
  calcBla5 <- function()return(list(x=new.magpie(years=1:2,fill=1),
                                    weight=new.magpie(years=3,fill=1),
                                    unit="1",
                                    description="test"))
  calcBla6 <- function()return(list(x=new.magpie(years=1:2,fill=1),
                                    weight=new.magpie(years=3,fill=1),
                                    description="test"))
  calcBla7 <- function()return(list(x=new.magpie(years=1:2,fill=1),
                                    weight=new.magpie(years=3,fill=1),
                                    unit="1"))
  calcBla8 <- function()return(list(x=new.magpie(years=1:2),
                                    weight=new.magpie(years=3,fill=1),
                                    unit="1",
                                    description="test"))
  calcBla9 <- function()return(list(x=new.magpie(years=1:2,fill=1),
                                    weight=new.magpie(years=3,fill=1),
                                    unit="1",
                                    description="test",
                                    max=0))
  calcBla10 <- function()return(list(x=new.magpie(years=1:2,fill=1),
                                    weight=new.magpie(years=3,fill=1),
                                    unit="1",
                                    description="test",
                                    min=10))
  calcBla11 <- function()return(list(x=1,
                                     class=list))
  calcBla12 <- function()return(list(x=1,
                                     class=c("classA","classB")))
  calcBla13 <- function()return(list(x=1,
                                     class="list"))
  calcBla14 <- function()return(list(x=list(1),
                                     class="list",
                                     unit="1",
                                     description="test"))
  globalassign(paste0("calcBla",1:14))
  
  expect_error(calcOutput("Bla1"),"not list of two MAgPIE objects")
  expect_error(calcOutput("Bla2"),"Output x of function .* is not a MAgPIE object")
  expect_error(calcOutput("Bla3"),"Output weight of function .* is not a MAgPIE object")
  expect_error(calcOutput("Bla4"),"Number of years disagree between data and weight")
  expect_error(calcOutput("Bla5"),"Neither .* contain a mapping compatible to the provided data")
  expect_warning(calcOutput("Bla6", aggregate=FALSE),"Missing unit information")
  expect_warning(calcOutput("Bla7", aggregate=FALSE),"Missing description")
  expect_warning(calcOutput("Bla8", aggregate=FALSE),"contains NAs")
  expect_warning(calcOutput("Bla9", aggregate=FALSE),"values greater than the predefined maximum")
  expect_warning(calcOutput("Bla10", aggregate=FALSE),"values smaller than the predefined minimum")
  expect_error(calcOutput("Bla11"),"class must be a single element of class character or NULL!")
  expect_error(calcOutput("Bla12"),"class must be a single element of class character or NULL!")
  expect_error(calcOutput("Bla13"),"Output x of function .* is not of promised class")
  expect_error(calcOutput("Bla14"),"Aggregation can only be used in combination with x\\$class=\"magpie\"")
  
  a <- calcOutput("Bla5", aggregate = FALSE)
  writeLines("CorruptCache", madrat:::cacheName("calc","Bla5",packages = "madrat", mode="get"))
  expect_warning(b <- calcOutput("Bla5", aggregate = FALSE),"corrupt cache")
  expect_identical(nc(a),nc(b))
  expect_identical(nc(b),nc(calcOutput("Bla5", aggregate = FALSE)))
  
  calcError <- function()stop("I am an error!")
  globalassign("calcError")
  expect_warning(suppressMessages(a <- calcOutput("Error", try=TRUE)), "I am an error", )
  expect_identical(class(a),"try-error")
})

test_that("Calculation for tau example data set works", {
  skip_if_offline()
  sink(tempfile())
  require(magclass)
  setConfig(enablecache = TRUE, forcecache=FALSE, verbosity = 2, mainfolder = tempdir())
  expected_result <- new("magpie", 
                         .Data = structure(c(0.99, 0.83, 0.68, 1.47, 0.9, 0.64, 0.8, 0.97, 1.17, 0.89, 1.27, 1.25),
                                           .Dim = c(12L, 1L, 1L), 
                                           .Dimnames = list(region = c("LAM", "OAS", "SSA", "EUR", "NEU", "MEA", 
                                                                       "REF", "CAZ", "CHA", "IND", "JPN", "USA"), 
                                                            year = NULL, data = NULL)))
  x <- calcOutput("TauTotal",source="historical",years = 1995, round=2, supplementary = TRUE) 
  expect_true(is.list(x))
  expect_equivalent(x$x,expected_result)
  expect_message(x <- readSource("Tau","historical"),"loading cache")
  expect_error(x <- readSource("Tau","wtf"),"Unknown subtype")
  expect_error(calcOutput("TauTotal",source = "historical", years = 1800), "Some years are missing")
  sink()
})


test_that("Standard workflow works", {
  
  setConfig(globalenv = TRUE,
            mainfolder = tempdir(),
            .verbose = FALSE)
  
  downloadTest2 <- function() {
    a <- as.magpie(1)
    getCells(a) <- "DEU"
    write.magpie(a,"test.mz")
  }
  readTest2 <- function() return(read.magpie("test.mz"))
  convertTest2 <- function(x) return(toolCountryFill(x, fill = 10))
  calcTest2 <- function() return(list(x=readSource("Test2"),
                                      weight=NULL,
                                      unit="1"))
  
  fullTEST2 <- function(rev=0, dev="") {
    expected_output <- new("magpie", 
                           .Data = structure(c(540, 490, 510, 331, 160, 210, 120, 50, 40, 10, 10, 10), 
                                             .Dim = c(12L, 1L, 1L), 
                                             .Dimnames = list(fake = c("LAM", "OAS", "SSA", "EUR", "NEU", "MEA", "REF", 
                                                                       "CAZ", "CHA", "IND", "JPN", "USA"), 
                                                              year = NULL, data = NULL)))
    
    expect_warning(co <- capture.output(a <- calcOutput("Test2", file="test.mz")),'Missing description for data set "Test2"')
    expect_equivalent(a, expected_output)
  }
  globalassign("downloadTest2", "readTest2", "convertTest2", "calcTest2", "fullTEST2")
  co <- capture.output(retrieveData("test2"))
})

test_that("Custom class support works", {
  setConfig(globalenv = TRUE, outputfolder = tempdir(), verbosity = 0, .verbose = FALSE)
  calcBla1 <- function()return(list(x          = list(1),
                                    class      = "list",
                                    unit       = "1",
                                    description = "test"))
  globalassign(paste0("calcBla",1))
  data <- calcOutput("Bla1", aggregate=FALSE, file = "test.rds")
  expect_equivalent(data, list(1))
  expect_identical(readRDS(paste0(getConfig("outputfolder"),"/test.rds")),data)
})

test_that("Old descriptors are properly removed from comment", {
  setConfig(globalenv = TRUE, outputfolder = tempdir(), verbosity = 0, .verbose = FALSE)
  calcBlub <- function() {
    x <- as.magpie(1)
    getComment(x) <- "test comment"
    return(list(x           = x,
                unit        = "1",
                description = "Descriptor test ",
                title       = "Blub"))
  }
  
  calcBlub2 <- function() {
    return(list(x           = calcOutput("Blub", aggregate=FALSE),
                unit        = "1",
                description = "Descriptor test 2",
                title       = "Blub2"))
  }
  
  globalassign("calcBlub", "calcBlub2")
  a <- calcOutput("Blub", aggregate=FALSE)
  expect_true(" comment: test comment" %in% getComment(a))
  a <- calcOutput("Blub2", aggregate=FALSE)
  expect_false(any(grepl("comment:", getComment(a))))
})

test_that("Aggregation works", {
  setConfig(globalenv = TRUE, outputfolder = tempdir(), verbosity = 0, .verbose = FALSE)
  calcAggregationTest <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description ="Aggregation test data",
                unit = "1"))
  }
  calcAggregationTest2 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description = "Aggregation test data 2",
                unit = "1",
                mixed_aggregation = TRUE,
                aggregationFunction = function(x,rel,mixed_aggregation) return(as.magpie(1))))
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
                aggregationFunction = function(x,rel,mixed_aggregation) return(as.magpie(1))))
  }
  globalassign("calcAggregationTest", "calcAggregationTest2",
               "calcMalformedAggregation","calcMalformedAggregation2")
  
  reg <- new("magpie", .Data = structure(c(54, 49, 51, 34, 16, 21, 12, 
                                           5, 4, 1, 1, 1), .Dim = c(12L, 1L, 1L), 
                                         .Dimnames = list(region = c("LAM","OAS", "SSA", "EUR", "NEU", 
                                                                     "MEA", "REF", "CAZ", "CHA", "IND", 
                                                                     "JPN", "USA"), 
                                                          year = NULL, data = NULL)))
  glo <- new("magpie", .Data = structure(249, .Dim = c(1L, 1L, 1L), 
              .Dimnames = list(region = "GLO", year = NULL, data = NULL)))

  expect_identical(nc(calcOutput("AggregationTest")), reg)
  expect_identical(nc(calcOutput("AggregationTest2")), clean_magpie(as.magpie(1)))
  expect_identical(nc(calcOutput("AggregationTest", aggregate="glo")), glo)
  expect_identical(nc(calcOutput("AggregationTest", aggregate="regglo")), mbind(reg,glo))
  expect_warning(a <- nc(calcOutput("AggregationTest", aggregate="global+region+cheese")), "Omitting cheese from aggregate")
  expect_identical(a,mbind(glo,reg))
  expect_error(calcOutput("MalformedAggregation"), "must be a function")
  expect_error(calcOutput("MalformedAggregation2"), "must be a list of function arguments")
  
  xtramap <- paste0(tempdir(),"/blub.csv")
  file.copy(toolGetMapping(getConfig("regionmapping"), returnPathOnly = TRUE), xtramap)
  setConfig(extramappings = xtramap)
  expect_warning(a <- nc(calcOutput("AggregationTest", aggregate="glo")), "Multiple compatible mappings found")
  expect_identical(a,glo)
  setConfig(extramappings = "")
})

test_that("Edge cases work as expected", {
  setConfig(globalenv = TRUE, outputfolder = tempdir(), verbosity = 0, .verbose = FALSE)
  calcEdgeTest <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description ="Aggregation test data",
                unit = "1"))
  }
  calcEdgeTest2 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    getYears(x) <- 2000
    return(list(x = x,
                weight = NULL,
                description ="Aggregation test data",
                unit = "1"))
  }
  calcNoMag <- function() {
    x <- list(1,2,3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description ="Non magclass test",
                unit = "1"))
  }
  calcNoMag2 <- function() {
    x <- list(1,2,3)
    return(list(x = x,
                weight = 12,
                class = "list",
                description ="Non magclass test",
                unit = "1"))
  }
  calcNoMag3 <- function() {
    x <- list(1,2,3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description ="Non magclass test",
                min = 0,
                unit = "1"))
  }
  calcNoMag4 <- function() {
    x <- list(1,2,3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description ="Non magclass test",
                structure.data = list(),
                unit = "1"))
  }
  calcNoMag5 <- function() {
    x <- list(1,2,3)
    return(list(x = x,
                weight = NULL,
                class = "list",
                description ="Non magclass test",
                isocountries = TRUE,
                unit = "1"))
  }
  globalassign("calcEdgeTest", "calcEdgeTest2", 
               "calcNoMag","calcNoMag2","calcNoMag3","calcNoMag4","calcNoMag5")
  expect_warning(calcOutput("EdgeTest", append = TRUE), "works only when the file name is provided")
  expect_warning(calcOutput("EdgeTest", file = "blub.mif"), "Time dimension missing and data cannot be written to a mif-file")
  a <- calcOutput("EdgeTest2", file = "blub.rds")
  expect_identical(a, readRDS(paste0(getConfig("outputfolder"),"/blub.rds")))
  
  expect_error(calcOutput("NoMag"), "Aggregation can only be used")
  expect_identical(nc(calcOutput("NoMag", aggregate = FALSE)), list(1,2,3))
  # now from cache...
  expect_identical(nc(calcOutput("NoMag", aggregate = FALSE)), list(1,2,3))
  expect_error(calcOutput("NoMag", aggregate = FALSE, round = 0), "rounding can only be used")
  expect_error(calcOutput("NoMag", aggregate = FALSE, years = 2000), "years argument can only be used")
  expect_error(calcOutput("NoMag", aggregate = FALSE, file = "bla.mz"), "Unsupported file format")
  expect_error(calcOutput("NoMag2", aggregate = FALSE),"Weights are currently not supported")
  expect_error(calcOutput("NoMag3", aggregate = FALSE),"Min/Max checks cannot be used")
  expect_error(calcOutput("NoMag4", aggregate = FALSE),"Structure checks cannot be used")
  expect_error(calcOutput("NoMag5", aggregate = FALSE),"isocountries can only be set if")
  
  skip_if_not_installed("reshape2")
  a <- calcOutput("EdgeTest2", file = "blub.mif")
  b <- magclass::read.report(paste0(getConfig("outputfolder"),"/blub.mif"), as.list = FALSE)
  expect_identical(sum(a - b), 0)
})

test_that("Data check works as expected", {
  calcMalformedISO <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                isocountries = 12,
                unit = "1"))
  }
  calcMalformedMixed <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                mixed_aggregation = 12,
                unit = "1"))
  }
  calcMalformedISO2 <- function() {
    x <- as.magpie(1)
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                isocountries = TRUE,
                unit = "1"))
  }
  calcMalformedISO3 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    getCells(x)[1] <- "BLA"
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                isocountries = TRUE,
                unit = "1"))
  }
  calcMalformedStruct <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                structure.spatial="ABC",
                unit = "1"))
  }
  calcMalformedStruct2 <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                structure.temporal="y[0-9]*",
                unit = "1"))
  }
  calcMatchingStruct <- function() {
    x <- new.magpie(getISOlist(), fill = 1)
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                structure.spatial="[A-Z]{3}",
                unit = "1"))
  }
  calcInfinite <- function() {
    x <- as.magpie(Inf)
    return(list(x = x,
                weight = NULL,
                description ="Malformed example",
                unit = "1"))
  }
  globalassign("calcMalformedISO","calcMalformedMixed",
               "calcMalformedISO2","calcMalformedISO3",
               "calcMalformedStruct","calcMalformedStruct2",
               "calcMatchingStruct", "calcInfinite")
  expect_error(calcOutput("MalformedISO"), "isocountries must be a logical")
  expect_error(calcOutput("MalformedMixed"), "mixed_aggregation must be a logical")
  expect_error(calcOutput("MalformedISO2"), "Wrong number of countries")
  expect_error(calcOutput("MalformedISO3"), "Countries .* do not agree with iso country list")
  expect_warning(calcOutput("MalformedStruct"), "Invalid names")
  expect_warning(calcOutput("MalformedStruct2"), "Missing names")
  expect_silent(suppressMessages(calcOutput("MatchingStruct")))
  cache <- madrat:::cacheName("calc","MatchingStruct")
  a <- readRDS(cache)
  getCells(a$x)[1] <- "BLA"
  saveRDS(a,cache)
  setConfig(verbosity = 2, .verbose = FALSE)
  expect_message(calcOutput("MatchingStruct"), "cache file corrupt")
  expect_warning(calcOutput("Infinite", aggregate = FALSE),"infinite values")
})


