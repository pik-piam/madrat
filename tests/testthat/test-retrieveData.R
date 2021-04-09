context("retrieveData")

test_that("retrieveData works as expected", {
  ofolder <- paste0(tempdir(),"/output")
  unlink(ofolder, recursive = TRUE)
  setConfig(outputfolder = ofolder, verbosity = 2, .verbose = FALSE)
  expect_message(retrieveData("example", rev = 0, dev = "test"), "execute function")
  expect_true(file.exists(paste0(getConfig("outputfolder"),"/rev0test_h12_example.tgz")))
})
