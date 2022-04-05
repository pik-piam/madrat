globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
}

setConfig(mainfolder =  withr::local_tempdir(), .verbose = FALSE)

test_that("retrieveData works as expected", {
  expect_message(retrieveData("example", rev = 0, dev = "test"), "Run retrieveData")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/rev0test_h12_5c275ce3_example_customizable_tag.tgz")))
  expect_message(retrieveData("example", rev = 0, dev = "test"), "data is already available")
})

test_that("retrieveData properly detects malformed inputs", {
  expect_error(retrieveData("NotThere"), "is not a valid output type")
  expect_error(retrieveData("Example", cachetype = "fantasy"), "Unknown cachetype")
  expect_error(retrieveData("Example", unknownargument = 42), "Unknown argument")
})

test_that("argument handling works", {
  fullTEST <- function(rev, myargument = NULL, forcecache = 12) {
    if (!is.null(myargument)) message("myargument = ", myargument)
    return()
  }
  globalassign("fullTEST")
  setConfig(globalenv = FALSE, .verbose = FALSE, .local = TRUE)
  expect_error(retrieveData("Test"), "is not a valid output type")
  expect_warning(retrieveData("Test", globalenv = TRUE), "Overlapping arguments")
  expect_error(retrieveData("Test"), "is not a valid output type") # globalenv is only set temporarily, so this fails
  expect_message(suppressWarnings(retrieveData("Test", myargument = "hello", globalenv = TRUE)),
                 "myargument = \"hello\"")
})

test_that("a tag can be appended to filename", {
  fullTESTTAG <- function() {
    return(list(tag = "some_tag"))
  }
  globalassign("fullTESTTAG")

  expect_message(retrieveData("TestTag", globalenv = TRUE), "Run retrieveData")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/rev0_h12_testtag_some_tag.tgz")))

  fullTESTTAG2 <- function() {
    return(list(tag = "debug_some_tag"))
  }
  globalassign("fullTESTTAG2")
  expect_warning(retrieveData("TestTag2", globalenv = TRUE), "should not include the word 'debug'")
})

test_that("retrieveData works if no tag is returned", {
  fullTESTTWO <- function() {
  }
  globalassign("fullTESTTWO")

  expect_message(retrieveData("Testtwo", globalenv = TRUE), "Run retrieveData")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/rev0_h12_testtwo.tgz")))
})

test_that("different kinds of arguments are logged correctly", {
  fullTEST <- function(arg1, arg2, arg3) {
  }
  globalassign("fullTEST")
  var1 <- c("bla", "blub")
  var2 <- list(bla = "blub", ble = 12)
  var3 <- as.magpie(1)
  expect_message(retrieveData("Test", globalenv = TRUE, arg1 = var1, arg2 = var2, arg3 = var3),
    paste(
      'Run retrieveData(model = "Test", puc = TRUE, globalenv = TRUE, arg1 = c("bla", "blub"),',
      'arg2 = list(bla = "blub", ble = 12), arg3 = new("magpie", .Data = 1))'
    ),
    fixed = TRUE
  )
})


test_that("strict mode works", {
  fullWARNTEST <- function() {
    calcOutput("WarningTest", aggregate = FALSE)
  }
  calcWarningTest <- function() {
    vcat(0, "This is a warning!")
    return(list(x = as.magpie(1), unit = "1", description = "dummy", isocountries = FALSE))
  }
  globalassign("fullWARNTEST", "calcWarningTest")
  setConfig(globalenv = TRUE, .verbose = FALSE, .local = TRUE)
  expect_warning(retrieveData("WarnTest", strict = TRUE, cachetype = "def"), "puc file not written")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/WARNINGS1_rev0_h12_warntest.tgz")))
})

rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
