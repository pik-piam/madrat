test_that("cacheCleanup deletes old files", {
  # Sys.setFileTime does not modify atime (only mtime/'last write time') on windows
  timeType <- if (identical(Sys.info()[["sysname"]], "Windows")) "mtime" else "atime"

  cacheFolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  cacheFile <- file.path(cacheFolder, "cacheFile")
  file.create(cacheFile)

  expect_error(cacheCleanup(30, cacheFolder, "stime"),
               "'arg' .*atime.*, .*mtime.*, .*ctime.*")

  cacheCleanup(30, cacheFolder, timeType, ask = FALSE)
  expect_true(file.exists(cacheFile))

  Sys.setFileTime(cacheFile, Sys.time() - 31 * 24 * 60 * 60) # set atime to 31 days ago

  expect_error({
    cacheCleanup(30, cacheFolder, readlineFunction = function(question) "asdf") # cryptic answer = FALSE
  }, "Please pass the correct path.", fixed = TRUE)

  cacheFileInfo <- cacheCleanup(30, cacheFolder, timeType, readlineFunction = function(question) {
    pathQuestion <- paste("Is the path correct?", cacheFolder, "(y/N) ")
    expect_true(question %in% c(pathQuestion,
                                "Do you want to delete these files? (y/N) "))
    if (identical(question, pathQuestion)) {
      return("YES")
    } else {
      return("") # default is no, should not delete file
    }
  })
  expect_true(file.exists(cacheFile))
  expectedFileInfo <- file.info(cacheFile)
  rownames(expectedFileInfo) <- basename(rownames(expectedFileInfo))
  expect_identical(cacheFileInfo, expectedFileInfo)

  cacheCleanup(30, cacheFolder, timeType, readlineFunction = function(question) "y")
  expect_false(file.exists(cacheFile))
})
