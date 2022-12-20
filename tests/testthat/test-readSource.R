cfg <- getConfig(verbose = FALSE)

nce <- function(x) {
  getComment(x) <- NULL
  attr(x, "cachefile") <- NULL
  attr(x, "id") <- NULL
  return(x)
}

test_that("readSource detects common problems", {
  localConfig(globalenv = TRUE, verbosity = 2, .verbose = FALSE)
  readNoDownload <- function() {} # nolint
  globalassign("readNoDownload")
  expect_error(readSource("NoDownload"), "no download script")

  downloadTest <- function() {
    return(list(url = "dummy", author = "dummy", title = "dummy", license = "dummy",
                description = "dummy", unit = "dummy"))
  }
  readTest <- function() return(1)
  globalassign("downloadTest", "readTest")
  expect_error(readSource("Test"),
               "Output of \"readTest()\" should have class \"magpie\" but it does not.", fixed = TRUE)
  readTest <- function(x) return(as.magpie(1))
  globalassign("readTest")
  expect_warning(readSource("Test", convert = FALSE), "Some arguments .* cannot be adressed by the wrapper")
  readTest <- function() return(as.magpie(1))
  convertTest <- function(x) return(as.magpie(1))
  globalassign("readTest", "convertTest")
  expect_error(readSource("Test"), "Wrong number of countries")
  expect_warning(readSource("Test", convert = "onlycorrect"), "No correct function .* could be found")
  correctTest <- function(x) return(as.magpie(1))
  globalassign("correctTest")
  expect_identical(as.vector(readSource("Test", convert = "onlycorrect")), 1)
  expect_message(readSource("Test", convert = "onlycorrect"), "loading cache")

  expect_error(readSource(TRUE), "Invalid type")
  expect_error(readSource("NonAvailable"), "not a valid source")

  readTest <- function() return(as.magpie(1))
  correctTest <- function(x) return(as.magpie(2))
  convertTest <- function(x) return(new.magpie(getISOlist(), fill = 1))
  globalassign("correctTest", "convertTest", "readTest")
  expect_identical(nce(readSource("Test", convert = FALSE)), clean_magpie(as.magpie(1)))
  expect_identical(nce(readSource("Test", convert = "onlycorrect")), clean_magpie(as.magpie(2)))
  expect_identical(nce(readSource("Test")), clean_magpie(new.magpie(getISOlist(), fill = 1)))

  cache <- madrat:::cacheName("convert", "Test")
  a <- readRDS(cache)
  getCells(a)[1] <- "BLA"
  saveRDS(a, cache)
  expect_message(readSource("Test"), "cache file corrupt")

  convertTest <- function(x) return(as.magpie(1))
  globalassign("convertTest")

  skip_on_cran()
  skip_if_offline("zenodo.org")
  expect_error(readSource("Tau", subtype = "paper", convert = "WTF"), "Unknown convert setting")
})

test_that("default readSource example works", {
  skip_on_cran()
  skip_if_offline("zenodo.org")
  expect_silent(suppressMessages({
    a <- readSource("Tau", "paper")
  }))
  expect_equal(getYears(a, as.integer = TRUE), c(1995, 2000))
})

test_that("downloadSource works", {
  skip_on_cran()
  skip_if_offline("zenodo.org")
  localConfig(globalenv = TRUE, verbosity = 2, .verbose = FALSE)
  expect_error(downloadSource("Tau", "paper"),
               paste('Source folder for source "Tau/paper" does already exist. Delete that folder or call',
                     "downloadSource(..., overwrite = TRUE) if you want to re-download."), fixed = TRUE)
  expect_error(downloadSource(1:10), "Invalid type")
  expect_error(downloadSource("Tau", subtype = 1:10), "Invalid subtype")
  downloadTest <- function() {
    return(list(url = 1, author = 1, title = 1, license = 1,
                description = 1, unit = 1, call = "notallowed"))
  }
  globalassign("downloadTest")
  expect_warning(downloadSource("Test", overwrite = TRUE), "reserved and will be overwritten")
})

test_that("forcecache works for readSource", {
  localConfig(mainfolder = withr::local_tempdir(), globalenv = TRUE)
  readTest2 <- function() new.magpie()
  globalassign("readTest2")
  expect_error(readSource("Test2"),
               paste('Sourcefolder does not contain data for the requested source type = "Test2" and there is no',
                     "download script which could provide the missing data. Please check your settings!"),
               fixed = TRUE)
  dir.create(file.path(getConfig("sourcefolder"), "Test2"), recursive = TRUE)
  expect_identical(readSource("Test2"), new.magpie())

  # ensure forced cache file is used even though sourcefolder does not exist
  unlink(file.path(getConfig("sourcefolder"), "Test2"), recursive = TRUE)
  localConfig(forcecache = TRUE)
  saveRDS("secret", cacheName("read", "Test2"))
  actual <- readSource("Test2")
  attributes(actual) <- NULL
  expect_identical(actual, "secret")
})

test_that("read functions can return non-magpie objects", {
  testReadSource <- function(readThis, correctThis = function(x) x, convertThis = function(x) x, convert = TRUE) {
    downloadThis <- function() list(url = "", author = "", title = "", license = "", description = "", unit = "")
    setConfig(globalenv = TRUE, .local = TRUE)
    stopifnot(!"This" %in% getCalculations(c("download", "read", "correct", "convert"))$type)
    withr::defer({
      rm("downloadThis", "readThis", "correctThis", "convertThis", envir = .GlobalEnv)
    })
    globalassign("downloadThis", "readThis", "correctThis", "convertThis")
    result <- readSource("This", convert = convert)
    return(result)
  }

  expect_identical(testReadSource(function() list(x = 1, class = "numeric")), list(x = 1, class = "numeric"))
  expect_error(testReadSource(function() list(x = 1, class = "character")),
               "Output of \"readThis()\" should have class \"character\" but it does not.",
               fixed = TRUE)
  expect_error(testReadSource(function() NULL),
               "Output of \"readThis()\" should have class \"magpie\" but it does not.",
               fixed = TRUE)
  expect_error(testReadSource(function() character(0)),
               "Output of \"readThis()\" should have class \"magpie\" but it does not.",
               fixed = TRUE)
  expect_error(testReadSource(function() list(y = 1, class = "character")),
               paste("Output of \"readThis()\" must be a MAgPIE object",
                     "or a list with the entries \"x\" and \"class\"!"), fixed = TRUE)

  # test whether ISO country check is applied
  expect_error(testReadSource(function() list(x = as.magpie(1), class = "magpie")),
               "Wrong number of countries returned by convertThis(x=x)!", fixed = TRUE)

  # test whether clean_magpie is applied
  brokenMagpie <- as.magpie(1)
  dimnames(brokenMagpie)[1] <- NULL
  expect_false(identical(brokenMagpie, clean_magpie(brokenMagpie)))
  expect_identical(testReadSource(function() list(x = brokenMagpie, class = "magpie"), convert = FALSE),
                   list(x = clean_magpie(brokenMagpie), class = "magpie"))
})

rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
