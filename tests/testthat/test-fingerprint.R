context("fingerprinting")

globalassign <- function(...) {
  for (x in c(...)) assign(x,eval.parent(parse(text = x)),.GlobalEnv)
}

test_that("fingerprinting works as expected", {
  toolTest <- function() {
    this <- 1
    is <- 2
    a <- 3
    test <- 4
    return(paste0(this, is, a, test))
  }
  globalassign("toolTest")
  expect_error(madrat:::fingerprint("toolTest", packages = "madrat", globalenv = FALSE), "There is no function with the name")
  expect_equivalent(madrat:::fingerprint("toolTest", packages = "madrat", globalenv = TRUE), "8b3413cc")
  emptyfolder <- paste0(tempdir(),"/empty")
  dir.create(emptyfolder, recursive = TRUE, showWarnings = FALSE)
  expect_equal(unname(madrat:::fingerprintFiles(emptyfolder)), "9ffb1463")
  unlink(emptyfolder)
})

test_that("fingerprintFiles works as expected", {
  setConfig(globalenv = TRUE, .verbose = FALSE, mainfolder = tempdir(), verbosity = 1)
  cwd <- getwd()
  setwd(tempdir())
  on.exit(setwd(cwd))
  writeLines("this is a test","test.txt")
  fp <- madrat:::fingerprintFiles("test.txt")
  expect_identical(fp, c(test.txt="515abfd5"))
})

test_that("fingerprinting works for edge cases", {
  setConfig(globalenv = TRUE, .verbose = FALSE, mainfolder = tempdir(), verbosity = 1)
  cwd <- getwd()
  setwd(tempdir())
  on.exit(setwd(cwd))
  writeLines("this is a test","map.csv")
  readFingerprintTest <- function() {
    map <- toolGetMapping("map.csv", where = "local")
    return(1)
  }
  globalassign("readFingerprintTest")
  expect_silent({fp <- madrat:::fingerprint("readFingerprintTest", packages = getConfig("packages"), details = TRUE)})
  expect_identical(attr(fp, "details")[-1], c("map.csv" = "0350ec95", readFingerprintTest = "b5efba0b"))
  expect_null(madrat:::fingerprintCall("blub"))
})

test_that("fingerprinting works with control flags", {
  setConfig(globalenv = TRUE, .verbose = FALSE, mainfolder = tempdir(), verbosity = 1)
  setConfig(globalenv = TRUE)
  readData <- function() return(1)
  readData2 <- function() return(2)
  calcExample2 <- function() {
    a <- readSource("Data")
    if(FALSE) b <- readSource("Data2")
  }
  globalassign("readData","readData2","calcExample2")
  fp_expected <- structure("7e60a810", call = "calcExample2", 
                                       details = c(calcExample2 = "8386790d", 
                                                   readData = "783a5e2f", 
                                                   readData2 = "fb52578f"))
  expect_identical(madrat:::fingerprint("calcExample2", details = TRUE, packages = "madrat"),fp_expected)
  readData3 <- function()return(3)
  calcExample2 <- function() {
    "!# @monitor readData3"
    "!# @ignore  readData2"
    a <- readSource("Data")
    if(FALSE) b <- readSource("Data2")
  }
  calcExample3 <- function() {
    a <- calcOutput("Example2", aggregate=FALSE)
  }
  calcExample4 <- function() {
    "!# @monitor readData2"
    "!# @ignore  readData2"
  }
  globalassign("readData3","calcExample2","calcExample3","calcExample4")
  
  fp2_expected <- structure("7b150c88", call = "calcExample2",
                                        details = c(calcExample2 = "ded5ab1f", 
                                    readData = "783a5e2f", readData3 = "2cd95ca8"))
  fp3_expected <- structure("edf89c03", call = "calcExample3",
                                        details = c(calcExample2 = "ded5ab1f", 
                                                    calcExample3 = "94819ad1", 
                                                    readData = "783a5e2f", 
                                                    readData2 = "fb52578f", 
                                                    readData3 = "2cd95ca8"))
  fp4_expected <- structure("91bbc100", call = "calcExample4",
                                        details = c(calcExample4 = "9aad0909", 
                                                    readData2 = "fb52578f"))
  expect_identical(madrat:::fingerprint("calcExample2", details = TRUE, packages = "madrat"), fp2_expected)
  expect_identical(madrat:::fingerprint("calcExample3", details = TRUE, packages = "madrat"), fp3_expected)
  expect_identical(madrat:::fingerprint("calcExample4", details = TRUE, packages = "madrat"), fp4_expected)
  rm(list = c("readData3","calcExample2","calcExample3","calcExample4"), envir = .GlobalEnv)
})



