test_that("cacheCleanup deletes old files", {
  expect_error(cacheCleanup(30, cacheFolder, "stime"),
               "'arg' should be one of .atime., .mtime., .ctime.")

  # Sys.setFileTime does not modify atime (only mtime/'last write time') on windows
  timeType <- if (identical(Sys.info()[["sysname"]], "Windows")) "mtime" else "atime"

  cacheFolder <- withr::local_tempdir()
  cacheFile <- file.path(cacheFolder, "cacheFile")
  file.create(cacheFile)

  cacheCleanup(30, cacheFolder, timeType, ask = FALSE)
  expect_true(file.exists(cacheFile))

  Sys.setFileTime(cacheFile, Sys.time() - 31 * 24 * 60 * 60) # set atime to 31 days ago

  cacheCleanup(30, cacheFolder, timeType, readlineFunction = function(question) {
    expect_identical(question,
                     "Do you want to delete these files? (y/N) ")
    return("") # default is no, should not delete file
  })
  expect_true(file.exists(cacheFile))

  expect_identical(cacheCleanup(30, cacheFolder, timeType, readlineFunction = function(question) "n"),
                   file.info(cacheFile))
  expect_true(file.exists(cacheFile))

  cacheCleanup(30, cacheFolder, timeType, readlineFunction = function(question) "y")
  expect_false(file.exists(cacheFile))
})
