context("retrieveData")

globalassign <- function(...) {
  for (x in c(...)) assign(x,eval.parent(parse(text = x)),.GlobalEnv)
}


test_that("retrieveData works as expected", {
  ofolder <- paste0(tempdir(),"/output")
  unlink(ofolder, recursive = TRUE)
  setConfig(outputfolder = ofolder, verbosity = 2, .verbose = FALSE)
  expect_message(retrieveData("example", rev = 0, dev = "test"), "execute function")
  expect_true(file.exists(paste0(getConfig("outputfolder"),"/rev0test_h12_example.tgz")))
  expect_message(retrieveData("example", rev = 0, dev = "test"), "data is already available")
})

test_that("retrieveData properly detects malformed inputs", {
  expect_error(retrieveData("NotThere"), "is not a valid output type")
  expect_error(retrieveData("Example", cachetype="fantasy"), "Unknown cachetype")
  expect_error(retrieveData("Example", unknownargument=42), "Unknown argument")
})  
  
test_that("argument handling works", {  
  fullTEST <- function(rev, myargument = NULL, forcecache=12) {
    if(!is.null(myargument)) message("myargument = ", myargument)
    return()
  }
  globalassign("fullTEST")
  setConfig(globalenv = FALSE, .verbose = FALSE)
  expect_error(retrieveData("Test"), "is not a valid output type")
  expect_warning(retrieveData("Test", globalenv = TRUE), "Overlapping arguments")
  expect_message(suppressWarnings(retrieveData("Test", myargument="hello")), "myargument = hello")
})
  