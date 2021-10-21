test_that("cacheCleanup deletes old files", {
  skip_if_not(endsWith(Sys.which("find"), "find") && Sys.which("touch") != "",
              "The GNU find or touch command line tool is not available.")

  cacheFolder <- withr::local_tempdir()
  cacheFile <- file.path(cacheFolder, "cacheFile")
  file.create(cacheFile)
  # get all files in cacheFolder accessed (atime = access time) during the last 24 hours (rounded down to 0 days)
  expect_identical(system2("find", c(shQuote(cacheFolder), "-type", "f", "-atime", "0"), stdout = TRUE), cacheFile)

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
