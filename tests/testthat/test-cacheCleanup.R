test_that("cacheCleanup deletes old files", {
  # skip_on_os("windows") # Sys.setFileTime does not modify atime (only mtime/'last write time') on windows
  # should also skip on systems where atimes are not available

  cacheFolder <- withr::local_tempdir()
  cacheFile <- file.path(cacheFolder, "cacheFile")
  file.create(cacheFile)

  cacheCleanup(30, cacheFolder, ask = FALSE)
  expect_true(file.exists(cacheFile))

  Sys.setFileTime(cacheFile, Sys.time() - 31 * 24 * 60 * 60) # set atime to 31 days ago

  cacheCleanup(30, cacheFolder, readlineFunction = function(question) {
    expect_identical(question, "Are you sure you want to delete these 1 files? (y/N) ")
    return("") # default is no, should not delete file
  })
  expect_true(file.exists(cacheFile))

  cacheCleanup(30, cacheFolder, readlineFunction = function(question) "y")
  expect_false(file.exists(cacheFile))
})
