context("fingerprinting")

test_that("fingerprinting works as expected", {
  expect_equal(madrat:::fingerprint("toolGetMapping", packages="madrat"), "203524aa")
  emptyfolder <- paste0(tempdir(),"/empty")
  dir.create(emptyfolder, recursive = TRUE)
  expect_equal(unname(madrat:::fingerprintFolder(emptyfolder)), "ca265a9c")
  unlink(emptyfolder)
})

