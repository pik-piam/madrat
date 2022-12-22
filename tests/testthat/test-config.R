test_that("cache folder can be set properly", {
  workingDirectory <- normalizePath(getwd(), winslash = "/")
  localConfig(cachefolder = workingDirectory, .verbose = FALSE)
  expect_identical(workingDirectory, getConfig("cachefolder"))

  # test shortcut formulation
  localConfig(cachefolder = "rev123", .verbose = FALSE)
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

test_that("localConfig only changes config temporarily", {
  cacheFolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  localConfig(cachefolder = cacheFolder)
  f <- function() {
    expect_identical(getConfig("cachefolder"), cacheFolder)
    cacheFolder2 <- normalizePath(withr::local_tempdir(), winslash = "/")
    expect_false(identical(cacheFolder, cacheFolder2))
    localConfig(cachefolder = cacheFolder2)
    expect_identical(getConfig("cachefolder"), cacheFolder2)
  }
  expect_identical(getConfig("cachefolder"), cacheFolder)
  f()
  expect_identical(getConfig("cachefolder"), cacheFolder)
})

test_that("main folder setting works", {
  withr::local_options(MADRAT_MAINFOLDER = NULL)
  withr::local_envvar(MADRAT_MAINFOLDER = NA)
  expect_identical(basename(getMainfolder(.testmode = TRUE)), "testmaindir")
  expect_identical(basename(getMainfolder()), "testmaindir")
})

test_that("addMapping works", {
  expect_null(getConfig("extramappings"))
  addMapping("regionmappingH12.csv")
  expect_identical(getConfig("extramappings"), "regionmappingH12.csv")
  map <- toolGetMapping("regionmappingH12.csv")
  expect_message(addMapping("test.rds", map), "extramappings: regionmappingH12.csv -> regionmappingH12.csv, test.rds")
  expect_identical(getConfig("extramappings"), c("regionmappingH12.csv", "test.rds"))
  expect_silent(map2 <- toolGetMapping("test.rds", type = "regional"))
  expect_identical(map, map2)
  expect_error(addMapping("test.blablub", map), "Unsupported filetype")
  expect_error(addMapping("blablub.csv", TRUE), "Cannot handle this mapping format")
})
