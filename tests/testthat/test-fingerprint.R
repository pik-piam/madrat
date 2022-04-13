globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
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
  expect_equivalent(madrat:::fingerprint("toolTest", packages = "madrat", globalenv = TRUE), "8b3413cc")
  emptyfolder <- paste0(tempdir(), "/empty")
  dir.create(emptyfolder, recursive = TRUE, showWarnings = FALSE)
  expect_equal(unname(madrat:::fingerprintFiles(emptyfolder)), "bc4159c0")
  unlink(emptyfolder)
})

test_that("fingerprintFiles works as expected", {
  setConfig(globalenv = TRUE, .verbose = FALSE, verbosity = 1, .local = TRUE)
  withr::local_dir(tempdir())
  writeLines("this is a test", "test.txt", sep = "")
  fp <- madrat:::fingerprintFiles("test.txt")
  expect_identical(fp, c(test.txt = "e495aa95"))
})

test_that("fingerprinting works for edge cases", {
  setConfig(globalenv = TRUE, .verbose = FALSE, verbosity = 1, .local = TRUE)
  withr::local_dir(tempdir())
  writeLines("this is a test", "map.csv", sep = "")
  readFingerprintTest <- function() {
    map <- toolGetMapping("map.csv", where = "local")
    return(1)
  }
  globalassign("readFingerprintTest")
  expect_silent({
    fp <- madrat:::fingerprint("readFingerprintTest", packages = getConfig("packages"), details = TRUE)
  })
  expect_identical(attr(fp, "details")[-1], c("map.csv" = "59eab5b3", readFingerprintTest = "b5efba0b"))
  expect_null(madrat:::fingerprintCall("blub"))
})

rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)

test_that("empty hash cache file is handled properly", {
  localTempdir <- withr::local_tempdir()
  dir.create(file.path(localTempdir, "something"))
  file.create(file.path(localTempdir, paste0("fileHashCache", basename(localTempdir), ".rds")))
  setConfig(sourcefolder = localTempdir, cachefolder = localTempdir, .local = TRUE)
  expect_warning(madrat:::fingerprintFiles(localTempdir),
                 "Ignoring corrupt hashCacheFile: Error in readRDS(hashCacheFile) : error reading from connection",
                 fixed = TRUE)
})

test_that("fingerprinting works with control flags", {
  setConfig(globalenv = TRUE, .verbose = FALSE, verbosity = 1, .local = TRUE)
  readData <- function() {
    return(1)
  }
  readData2 <- function() {
    return(2)
  }
  calcExample2 <- function() {
    a <- readSource("Data")
    if (FALSE) b <- readSource("Data2")
  }
  globalassign("readData", "readData2", "calcExample2")
  fpExpected <- structure("a18f3e07",
    call = "calcExample2",
    details = c(
      calcExample2 = "8386790d",
      readData = "667d53aa",
      readData2 = "041bef7f"
    )
  )
  expect_identical(madrat:::fingerprint("calcExample2", details = TRUE, packages = "madrat"), fpExpected)
  readData3 <- function() {
    return(3)
  }
  calcExample2 <- function() {
    "!# @monitor readData3"
    "!# @ignore  readData2"
    a <- readSource("Data")
    if (FALSE) b <- readSource("Data2")
  }
  calcExample3 <- function() {
    a <- calcOutput("Example2", aggregate = FALSE)
  }
  calcExample4 <- function() {
    "!# @monitor readData2"
    "!# @ignore  readData2"
  }
  globalassign("readData3", "calcExample2", "calcExample3", "calcExample4")

  fp2Expected <- structure("4b34fe34",
    call = "calcExample2",
    details = c(
      calcExample2 = "ded5ab1f",
      readData = "667d53aa", readData3 = "56324bcf"
    )
  )
  fp3Expected <- structure("a2236072",
    call = "calcExample3",
    details = c(
      calcExample2 = "ded5ab1f",
      calcExample3 = "94819ad1",
      readData = "667d53aa",
      readData2 = "041bef7f",
      readData3 = "56324bcf"
    )
  )
  fp4Expected <- structure("1def7910",
    call = "calcExample4",
    details = c(
      calcExample4 = "9aad0909",
      readData2 = "041bef7f"
    )
  )
  expect_identical(madrat:::fingerprint("calcExample2", details = TRUE, packages = "madrat"), fp2Expected)
  expect_identical(madrat:::fingerprint("calcExample3", details = TRUE, packages = "madrat"), fp3Expected)
  expect_identical(madrat:::fingerprint("calcExample4", details = TRUE, packages = "madrat"), fp4Expected)
})

rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
