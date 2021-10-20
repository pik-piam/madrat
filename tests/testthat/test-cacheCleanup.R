test_that("cacheCleanup deletes old files", {
  cacheFolder <- withr::local_tempdir()

  if (!commandAvailable("find")) {
    expect_error(cacheCleanup(30, cacheFolder, ask = FALSE),
                 "cacheCleanup requires the find command, but it is not available on your system.", fixed = TRUE)
  }

  skip_if_not(commandAvailable("find") && commandAvailable("touch"))

  cacheFile <- file.path(cacheFolder, "cacheFile")
  file.create(cacheFile)
  # get all files in cacheFolder accessed (atime = access time) during the last 24 hours
  expect_identical(system2("find", c(shQuote(cacheFolder), "-type", "f", "-atime", "0"), stdout = TRUE),
                   cacheFile)

  cacheCleanup(30, cacheFolder, ask = FALSE)
  expect_true(file.exists(cacheFile))

  system2("touch", c("--date='31 days ago'", shQuote(cacheFile))) # set atime to 31 days ago
  expect_length(system2("find", c(shQuote(cacheFolder), "-type", "f", "-atime", "0"), stdout = TRUE), 0)

  cacheCleanup(30, cacheFolder, readlineFunction = function(question) {
    expect_identical(question, paste0("The following files are older than 30 days:\n",
                                      cacheFile, "\n",
                                      "Are you sure you want to delete these files? (y/N) "))
    return("") # default is no, should not delete file
  })
  expect_true(file.exists(cacheFile))

  cacheCleanup(30, cacheFolder, readlineFunction = function(question) "y")
  expect_false(file.exists(cacheFile))
})
