context("fingerprinting")

test_that("fingerprinting works as expected", {
  print(eval(parse(text="madrat:::toolGetMapping")))
  print(fingerprintFunction("madrat:::toolGetMapping"))
  print(digest(unname(fingerprintFunction("madrat:::toolGetMapping")), algo = "xxhash32"))
  expect_equal(madrat:::fingerprint("toolGetMapping", packages="madrat"), "203524aa")
  emptyfolder <- paste0(tempdir(),"/empty")
  dir.create(emptyfolder, recursive = TRUE)
  expect_equal(unname(madrat:::fingerprintFolder(emptyfolder)), "ca265a9c")
  unlink(emptyfolder)
})

globalassign <- function(...) {
  for(x in c(...)) assign(x,eval.parent(parse(text=x)),.GlobalEnv)
}

test_that("fingerprinting works for edge cases", {
  setConfig(globalenv = TRUE, .verbose = FALSE, mainfolder=tempdir())
  readTest <- function()return(1)
  globalassign("readTest")
  expect_silent(madrat:::fingerprint("readTest", packages = getConfig("packages")))
  
})

