globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
}


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

test_that("set extramapping via setConfig and add it's hash to filename", {
  fullTESTX <- function() {
    return()
  }
  globalassign("fullTESTX")
  setConfig("nolabels" = TRUE)
  setConfig("regionmapping" = "regionmappingH12.csv")
  setConfig("extramappings" = "regionmapping_21_EU11-columnname.csv")
  expect_message(retrieveData("TestX", globalenv = TRUE), "Run retrieveData")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/rev0_62eff8f7-ba755b60_testx.tgz")))
  setConfig("extramappings" = "")
  expect_identical(getConfig("extramappings"), NULL)
  setConfig("nolabels" = FALSE)
  expect_identical(getConfig("nolabels"), FALSE)
})

test_that("set extramapping via retrieveData and add it's hash and the hash of the full function to filename", {
  fullTESTY <- function(dummy=FALSE) {
    # this function has a dummy parameter to yield a hash that will also be added to the name of the tgz file
    return()
  }
  globalassign("fullTESTY")
  setConfig("extramappings" = "")
  setConfig("nolabels" = TRUE)
  expect_identical(getConfig("extramappings"), NULL)
  expect_message(retrieveData("TestY", dummy = TRUE,
                              regionmapping = "regionmappingH12.csv",
                              extramappings = "regionmapping_21_EU11-columnname.csv", globalenv = TRUE),
                 "Run retrieveData")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/rev0_62eff8f7-ba755b60_2d7a1a9c_testy.tgz")))
  setConfig("nolabels" = FALSE)
  expect_identical(getConfig("nolabels"), FALSE)
})

test_that("a tag can be appended to filename", {
  fullTEST <- function() {
    return(list(tag = "some_tag"))
  }
  globalassign("fullTEST")

  expect_message(retrieveData("Test", globalenv = TRUE), "Run retrieveData")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/rev0_h12_test_some_tag.tgz")))

  fullTEST2 <- function() {
    return(list(tag = "debug_some_tag"))
  }
  globalassign("fullTEST2")
  expect_warning(retrieveData("Test2", globalenv = TRUE), "should not include the word 'debug'")
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

rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
