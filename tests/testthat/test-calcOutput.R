context("Data calculation wrapper")

cfg <- getConfig(verbose = FALSE)

globalassign <- function(...) {
  for(x in c(...)) assign(x,eval.parent(parse(text=x)),.GlobalEnv)
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
  expect_error(setConfig(packages="nonexistentpackage"),'Setting "packages" can only be set to installed packages')
  expect_error(calcOutput("TauTotal",aggregate = "wtf"), "Illegal setting aggregate = wtf")
  expect_error(calcOutput(TRUE), "Invalid type \\(must be a character\\)")
  expect_error(calcOutput(c("a","b")), "Invalid type \\(must be a single character string\\)")
})

test_that("Malformed calc outputs are properly detected", {
  sink(tempfile())
  setConfig(globalenv = TRUE, .verbose = FALSE)
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
  globalassign(paste0("calcBla",1:10))
  
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

  a <- calcOutput("Bla5", aggregate=FALSE)
  setConfig(forcecache = TRUE)
  writeLines("CorruptCache", paste0(getConfig("cachefolder"),"/calcBla5.rds"))
  expect_warning(b <- calcOutput("Bla5", aggregate=FALSE),"readRDS")
  expect_identical(a,b)
  expect_identical(b,calcOutput("Bla5", aggregate=FALSE))
  sink()
})

test_that("Calculation for tau example data set works", {
  sink(tempfile())
  expected_result <- new("magpie", 
                         .Data = structure(c(0.99, 0.83, 0.68, 1.47, 0.9, 0.64, 0.8, 0.97, 1.17, 0.89, 1.27, 1.25),
                                           .Dim = c(12L, 1L, 1L), 
                                           .Dimnames = list(region = c("LAM", "OAS", "SSA", "EUR", "NEU", "MEA", 
                                                                       "REF", "CAZ", "CHA", "IND", "JPN", "USA"), 
                                                            year = NULL, data = NULL)))
  x <- calcOutput("TauTotal",source="historical",years = 1995, round=2, supplementary = TRUE) 
  expect_true(is.list(x))
  expect_equivalent(x$x,expected_result)
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
