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
  expect_equal(madrat:::fingerprint("toolTest", packages = "madrat", globalenv = TRUE), "8b3413cc")
  emptyfolder <- paste0(tempdir(),"/empty")
  dir.create(emptyfolder, recursive = TRUE, showWarnings = FALSE)
  expect_equal(unname(madrat:::fingerprintFiles(emptyfolder, use.mtime = TRUE)), "ca265a9c")
  unlink(emptyfolder)
})

test_that("fingerprintFiles works as expected", {
  setConfig(globalenv = TRUE, .verbose = FALSE, mainfolder = tempdir(), verbosity = 1)
  cwd <- getwd()
  setwd(tempdir())
  on.exit(setwd(cwd))
  writeLines("this is a test","test.txt")
  fp <- madrat:::fingerprintFiles("test.txt", use.mtime = FALSE)
  expect_identical(fp, c(test.txt="7e76cec7"))
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
  expect_identical(attr(fp,"details")[-1], c(readFingerprintTest="b5efba0b","map.csv"="7e76cec7"))
})



