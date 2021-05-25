globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
}


test_that("retrieveData works as expected", {
  expect_message(retrieveData("example", rev = 0, dev = "test"), "Run retrieveData")
  expect_true(file.exists(paste0(getConfig("outputfolder"), "/rev0test_h12_example_customizable_tag.tgz")))
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
  setConfig(globalenv = FALSE, .verbose = FALSE)
  expect_error(retrieveData("Test"), "is not a valid output type")
  expect_warning(retrieveData("Test", globalenv = TRUE), "Overlapping arguments")
  expect_message(suppressWarnings(retrieveData("Test", myargument = "hello")), "myargument = \"hello\"")
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
      'Run retrieveData(model = "Test", globalenv = TRUE, arg1 = c("bla", "blub"),',
      'arg2 = list(bla = "blub", ble = 12), arg3 = new("magpie", .Data = 1))'
    ),
    fixed = TRUE
  )
})
