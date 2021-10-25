test_that("findOldFiles finds old files", {
  # Sys.setFileTime does not modify atime (only mtime/'last write time') on windows
  timeType <- if (identical(Sys.info()[["sysname"]], "Windows")) "mtime" else "atime"

  cacheFolder <- withr::local_tempdir()
  cacheFile <- normalizePath(file.path(cacheFolder, "cacheFile"), winslash = "/", mustWork = FALSE)
  file.create(cacheFile)

  expect_identical(nrow(findOldFiles(30, cacheFolder, timeType)), 0L)
  Sys.setFileTime(cacheFile, Sys.time() - 31 * 24 * 60 * 60) # set atime to 31 days ago
  expect_identical(rownames(findOldFiles(30, cacheFolder, timeType)), cacheFile)

  expect_error(findOldFiles(30, cacheFolder, "stime"),
               "'arg' should be one of .atime., .mtime., .ctime.")
})
