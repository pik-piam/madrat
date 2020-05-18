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
