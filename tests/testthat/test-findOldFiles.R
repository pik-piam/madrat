test_that("findOldFiles finds old files", {
  # skip_on_os("windows") # Sys.setFileTime does not modify atime (only mtime/'last write time') on windows
  # should also skip on systems where atimes are not available

  cacheFolder <- withr::local_tempdir()
  cacheFile <- normalizePath(file.path(cacheFolder, "cacheFile"), winslash = "/", mustWork = FALSE)
  file.create(cacheFile)

  expect_identical(nrow(findOldFiles(30, cacheFolder)), 0L)
  Sys.setFileTime(cacheFile, Sys.time() - 31 * 24 * 60 * 60) # set atime to 31 days ago
  expect_identical(rownames(findOldFiles(30, cacheFolder)), cacheFile)
})
