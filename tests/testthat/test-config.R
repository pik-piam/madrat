test_that("cache folder can be set properly", {
  workingDirectory <- normalizePath(getwd(), winslash = "/")
  setConfig(cachefolder = workingDirectory, .verbose = FALSE, .local = TRUE)
  expect_identical(workingDirectory, getConfig("cachefolder"))

  # test shortcut formulation
  setConfig(cachefolder = "rev123", .verbose = FALSE, .local = TRUE)
  expect_identical(normalizePath(file.path(getConfig("mainfolder"), "cache", "rev123"), winslash = "/"),
                   getConfig("cachefolder"))
})

test_that("setConfig(..., .local = TRUE) only changes config temporarily", {
  cacheFolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  setConfig(cachefolder = cacheFolder, .local = TRUE)
  f <- function() {
    expect_identical(getConfig("cachefolder"), cacheFolder)
    cacheFolder2 <- normalizePath(withr::local_tempdir(), winslash = "/")
    expect_false(identical(cacheFolder, cacheFolder2))
    setConfig(cachefolder = cacheFolder2, .local = TRUE)
    expect_identical(getConfig("cachefolder"), cacheFolder2)
  }
  expect_identical(getConfig("cachefolder"), cacheFolder)
  f()
  expect_identical(getConfig("cachefolder"), cacheFolder)
})

test_that("main folder setting works", {
  withr::local_options(MADRAT_MAINFOLDER = NULL)
  withr::local_envvar(MADRAT_MAINFOLDER = NA)
  expect_identical(basename(madrat:::getMainfolder(.testmode = TRUE)), "testmaindir")
  expect_identical(basename(madrat:::getMainfolder()), "testmaindir")

  withr::local_options(MADRAT_MAINFOLDER = NULL)
  withr::local_envvar(MADRAT_MAINFOLDER = "test")
  expect_identical(basename(madrat:::getMainfolder()), "test")
})
