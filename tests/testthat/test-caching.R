test_that("Caching works", {
  calcCacheExample <- function() return(list(x = as.magpie(1), description = "-", unit = "-"))
  globalassign("calcCacheExample")
  localConfig(ignorecache = FALSE, .verbose = FALSE)
  expect_null(cacheGet("calc", "CacheExample"))
  expect_message(calcOutput("CacheExample", aggregate = FALSE), "writing cache")
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
  readCacheExample <- function(subtype = "bla") as.magpie(1)
  correctCacheExample <- function(x, subtype = "blub") {
    if (subtype == "blub") return(as.magpie(1))
    else if (subtype == "bla") return(as.magpie(2))
  }
  globalassign("downloadCacheExample", "readCacheExample", "correctCacheExample")
  expect_message(readSource("CacheExample", convert = "onlycorrect"), "correctCacheExample-F[^-]*.rds")
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
