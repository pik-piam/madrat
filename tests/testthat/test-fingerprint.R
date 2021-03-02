context("fingerprinting")


globalassign <- function(...) {
  for(x in c(...)) assign(x,eval.parent(parse(text=x)),.GlobalEnv)
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
  expect_error(madrat:::fingerprint("toolTest", packages="madrat", globalenv=FALSE), "There is no function with the name")
  expect_equal(madrat:::fingerprint("toolTest", packages="madrat", globalenv=TRUE), "27e9ba9e")
  emptyfolder <- paste0(tempdir(),"/empty")
  dir.create(emptyfolder, recursive = TRUE)
  expect_equal(unname(madrat:::fingerprintFolder(emptyfolder)), "ca265a9c")
  unlink(emptyfolder)
})

test_that("fingerprinting works for edge cases", {
  setConfig(globalenv = TRUE, .verbose = FALSE, mainfolder=tempdir())
  readTest <- function()return(1)
  globalassign("readTest")
  expect_silent(madrat:::fingerprint("readTest", packages = getConfig("packages")))
  
})

