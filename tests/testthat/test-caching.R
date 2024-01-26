test_that("Caching works", {
  calcCacheExample <- function() return(list(x = as.magpie(1), description = "-", unit = "-"))
  calcNoCacheExample <- function() return(list(x = as.magpie(1), description = "-", unit = "-", cache = FALSE))
  downloadNoCacheExample <- function() {
    return(list(url = 0, author = 0, title = 0, license = 0, description = 0, unit = 0))
  }
  readNoCacheExample <- function() return(list(x = as.magpie(1), class = "magpie", cache = FALSE))
  globalassign("calcCacheExample", "calcNoCacheExample", "readNoCacheExample", "downloadNoCacheExample")
  localConfig(ignorecache = FALSE, .verbose = FALSE)
  expect_null(cacheGet("calc", "CacheExample"))
  expect_message(calcOutput("CacheExample", aggregate = FALSE), "writing cache")
  expect_message(calcOutput("NoCacheExample", aggregate = FALSE), "cache disabled for calcNoCacheExample")
  expect_message(readSource("NoCacheExample", convert = FALSE), "cache disabled for readNoCacheExample")
  expect_identical(cacheGet("calc", "CacheExample")$x, as.magpie(1))
  localConfig(ignorecache = TRUE, .verbose = FALSE)
  expect_null(cacheGet("calc", "CacheExample"))
  localConfig(ignorecache = FALSE, .verbose = FALSE)

  expect_identical(basename(cacheName("calc", "CacheExample")), "calcCacheExample-Ff5d41fca.rds")

  calcCacheExample <- function() return(list(x = as.magpie(2), description = "-", unit = "-"))
  globalassign("calcCacheExample")
  expect_null(cacheName("calc", "CacheExample", mode = "get"))
  localConfig(forcecache = TRUE, .verbose = FALSE)
  expect_identical(basename(cacheName("calc", "CacheExample")), "calcCacheExample.rds")
  expect_message(cf <- cacheName("calc", "CacheExample", mode = "get"), "does not match fingerprint")
  expect_identical(basename(cf), "calcCacheExample-Ff5d41fca.rds")
  localConfig(forcecache = FALSE, .verbose = FALSE)
  Sys.sleep(1) # wait a second to ensure this second cache file has newer mtime, so forcecache reproducibly takes it
  expect_message(a <- calcOutput("CacheExample", aggregate = FALSE), "writing cache")
  expect_identical(basename(cacheName("calc", "CacheExample", mode = "get")), "calcCacheExample-Fad6287a7.rds")

  calcCacheExample <- function() return(list(x = as.magpie(3), description = "-", unit = "-"))
  globalassign("calcCacheExample")
  localConfig(forcecache = TRUE, .verbose = FALSE)
  expect_message(cf <- cacheName("calc", "CacheExample", mode = "get"), "does not match fingerprint")
  expect_identical(basename(cf), "calcCacheExample-Fad6287a7.rds")
})

test_that("Argument hashing works", {
  expect_null(cacheArgumentsHash(readTau))
  expect_null(cacheArgumentsHash(readTau, list(subtype = "paper")))
  expect_identical(cacheArgumentsHash(readTau, args = list(subtype = "historical")), "-50d72f51")
  expect_identical(cacheArgumentsHash(c(readTau, convertTau),
                                      args = list(subtype = "historical")), "-50d72f51")
  # nonexisting arguments will be ignored if ... is missing
  expect_identical(cacheArgumentsHash(readTau, args = list(subtype = "historical", notthere = 42)),
                   "-50d72f51")
  # if ... exists all arguments will get considered
  expect_null(cacheArgumentsHash(calcOutput, args = list(try = FALSE)))
  expect_identical(cacheArgumentsHash(calcOutput, args = list(try = TRUE)), "-01df3eb2")
  expect_identical(cacheArgumentsHash(calcOutput, args = list(try = TRUE, notthere = 42)), "-ae021eac")
  calcArgs <- function(a = NULL) return(1)
  expect_null(cacheArgumentsHash(calcArgs))
  expect_null(cacheArgumentsHash(calcArgs, args = list(a = NULL)))
  expect_identical(cacheArgumentsHash(calcArgs, args = list(a = 12)), "-8bb64daf")
  expect_error(cacheArgumentsHash(NULL, args = list(no = "call")), "No call")
})

test_that("Cache naming and identification works correctly", {
  localConfig(forcecache = FALSE, .verbose = FALSE)
  downloadCacheExample <- function() {
    return(list(url = 1, author = 1, title = 1, license = 1, description = 1, unit = 1))
  }
  readCacheExample <- function(subtype = "blub") as.magpie(1)
  correctCacheExample <- function(x, subtype = "blub") {
    if (subtype == "blub") return(as.magpie(1))
    else if (subtype == "bla") return(as.magpie(2))
  }
  globalassign("downloadCacheExample", "readCacheExample", "correctCacheExample")
  expect_message(readSource("CacheExample", subtype = "blub", convert = "onlycorrect"),
                 "writing cache correctCacheExample-F[^-]*.rds")
  expect_message(readSource("CacheExample", convert = "onlycorrect"),
                 "loading cache correctCacheExample-F[^-]*.rds")
  expect_message(readSource("CacheExample", convert = "onlycorrect", subtype = "bla"),
                 "correctCacheExample-F[^-]*-d0d19d80.rds")
  expect_message(readSource("CacheExample", convert = "onlycorrect", subtype = "blub"),
                 "correctCacheExample-F[^-]*.rds")

  readCacheExample <- function(subtype = "blub") {
    if (subtype == "blub") return(as.magpie(1))
    else if (subtype == "bla") return(as.magpie(2))
  }
  correctCacheExample <- function(x) return(x)

  globalassign("downloadCacheExample", "readCacheExample", "correctCacheExample")
  expect_message(readSource("CacheExample", convert = "onlycorrect"), "correctCacheExample-F[^-]*.rds")
  expect_message(readSource("CacheExample", convert = "onlycorrect", subtype = "bla"),
                 "correctCacheExample-F[^-]*-d0d19d80.rds")
  expect_message(readSource("CacheExample", convert = "onlycorrect", subtype = "blub"),
                 "correctCacheExample-F[^-]*.rds")
})

test_that("non-list cache files are supported for forcecache", {
  localConfig(cachefolder = withr::local_tempdir(), forcecache = TRUE)

  # write legacy non-list cache file
  saveRDS(as.magpie(1), file.path(getConfig("cachefolder"), "readCacheExample-Fasdasd.rds"))

  readCacheExample <- function() as.magpie(1)
  globalassign("readCacheExample")

  expect_identical(readSource("CacheExample", supplementary = TRUE),
                   list(x = readSource("CacheExample"), class = "magpie"))
})

test_that("terra objects can be cached", {
  skip_if_not_installed("terra")

  downloadSingleSource <- function() {
    return(list(url = 0, author = 0, title = 0, license = 0, description = 0, unit = 0))
  }
  readSingleSource <- function() {
    x <- terra::rast(system.file("ex/meuse.tif", package = "terra"))
    names(x) <- "something"
    terra::units(x) <- "some unit"
    terra::time(x) <- 1234
    return(list(x = x, class = "SpatRaster"))
  }
  globalassign("downloadSingleSource", "readSingleSource")
  expect_message(a <- readSource("SingleSource"), "writing cache")
  expect_message(b <- readSource("SingleSource"), "loading cache")
  # converting to data frame because terra::sources is different
  expect_equal(terra::as.data.frame(a, xy = TRUE),
               terra::as.data.frame(b, xy = TRUE))
  expect_identical(names(a), names(b))
  expect_identical(terra::units(a), terra::units(b))
  expect_equal(terra::time(a), terra::time(b))


  downloadInMemory <- function() {
    return(list(url = 0, author = 0, title = 0, license = 0, description = 0, unit = 0))
  }
  readInMemory <- function() {
    x <- terra::rast(system.file("ex/meuse.tif", package = "terra"))
    x <- x * 2
    names(x) <- "something"
    return(list(x = x, class = "SpatRaster"))
  }
  globalassign("downloadInMemory", "readInMemory")
  expect_message(a <- readSource("InMemory"), "writing cache")
  expect_message(b <- readSource("InMemory"), "loading cache")
  # converting to data frame because terra::sources is different
  expect_equal(terra::as.data.frame(a, xy = TRUE),
               terra::as.data.frame(b, xy = TRUE))
  expect_identical(names(a), names(b))


  downloadMultiSource <- function() {
    return(list(url = 0, author = 0, title = 0, license = 0, description = 0, unit = 0))
  }
  readMultiSource <- function() {
    a <- terra::rast(system.file("ex/meuse.tif", package = "terra"))
    a <- c(a, a)
    names(a) <- c("something", "else")
    return(list(x = a, class = "SpatRaster"))
  }
  globalassign("downloadMultiSource", "readMultiSource")
  expect_message(a <- readSource("MultiSource"), "writing cache")
  expect_message(b <- readSource("MultiSource"), "loading cache")
  # converting to data frame because terra::sources is different
  expect_equal(terra::as.data.frame(a, xy = TRUE),
               terra::as.data.frame(b, xy = TRUE))
  expect_identical(names(a), names(b))

  readMultiSource <- function() {
    a <- terra::rast(system.file("ex/meuse.tif", package = "terra"))
    a <- c(a, a * 2) # one SpatRaster from source file, one in-memory
    return(list(x = a, class = "SpatRaster"))
  }
  globalassign("readMultiSource")
  expect_warning(readSource("MultiSource"),
                 "file-based and in-memory parts in the same terra object can currently not be cached")


  downloadSpatVector <- function() {
    return(list(url = 0, author = 0, title = 0, license = 0, description = 0, unit = 0))
  }
  readSpatVector <- function() {
    return(list(x = terra::vect(system.file("ex/lux.shp", package = "terra")),
                class = "SpatVector"))
  }
  globalassign("downloadSpatVector", "readSpatVector")
  expect_message(a <- readSource("SpatVector"), "writing cache")
  expect_message(b <- readSource("SpatVector"), "loading cache")
  # converting to data frame because terra::sources is different
  expect_equal(terra::as.data.frame(a, geom = "WKT"),
               terra::as.data.frame(b, geom = "WKT"))
  expect_identical(names(a), names(b))


  downloadInMemoryVector <- function() {
    return(list(url = 0, author = 0, title = 0, license = 0, description = 0, unit = 0))
  }
  readInMemoryVector <- function() {
    return(list(x = terra::vect("POLYGON ((0 -5, 10 0, 10 -10, 0 -5))"),
                class = "SpatVector"))
  }
  globalassign("downloadInMemoryVector", "readInMemoryVector")
  expect_message(a <- readSource("InMemoryVector"), "writing cache")
  expect_message(b <- readSource("InMemoryVector"), "loading cache")
  # converting to data frame because terra::sources is different
  expect_equal(terra::as.data.frame(a, geom = "WKT"),
               terra::as.data.frame(b, geom = "WKT"))
  expect_identical(names(a), names(b))
})
