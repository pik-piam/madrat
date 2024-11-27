nce <- function(x) {
  if (is.list(x)) {
    x$x <- nce(x$x)
  }
  attr(x, "comment") <- NULL
  attr(x, "cachefile") <- NULL
  attr(x, "id") <- NULL
  return(x)
}

test_that("readSource detects common problems", {
  localConfig(verbosity = 2, .verbose = FALSE)
  readNoDownload <- function() NULL
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

  cache <- cacheName("convert", "Test")
  a <- readRDS(cache)
  getCells(a$x)[1] <- "BLA"
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
  localConfig(verbosity = 2, .verbose = FALSE)
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
  localConfig(mainfolder = withr::local_tempdir())
  readTest2 <- function() new.magpie()
  globalassign("readTest2")
  expect_error(readSource("Test2"),
               paste('Sourcefolder does not contain data for the requested source type = "Test2" and there is no',
                     "download script which could provide the missing data. Please check your settings!"),
               fixed = TRUE)
  dir.create(file.path(getConfig("sourcefolder"), "Test2"), recursive = TRUE)
  expect_identical(nce(readSource("Test2")), new.magpie())

  # ensure forced cache file is used even though sourcefolder does not exist
  unlink(file.path(getConfig("sourcefolder"), "Test2"), recursive = TRUE)
  localConfig(forcecache = TRUE)
  saveRDS("secret", cacheName("read", "Test2"))
  actual <- readSource("Test2")
  attributes(actual) <- NULL
  expect_identical(actual, "secret")
})

test_that("read functions can handle metadata", {
  dir.create(file.path(getConfig("sourcefolder"), "MetadataTest"), recursive = TRUE)
  readMetadataTest <- function() {
    return(list(x = as.magpie(1),
                description = "Metadata Test",
                unit = "kg"))
  }
  correctMetadataTest <- function(x) {
    expect_identical(getFromComment(x, "description"), "Metadata Test")
    expect_identical(getFromComment(x, "unit"), "kg")
    expect_null(getFromComment(x, "NonExisting"))
    return(list(x = x, description = "Metadata Test 2", unit = "ton"))
  }
  globalassign("readMetadataTest", "correctMetadataTest")
  x <- readSource("MetadataTest", convert = "onlycorrect")
  expect_identical(getFromComment(x, "description"), "Metadata Test 2")
  expect_identical(getFromComment(x, "unit"), "ton")
  expect_null(getFromComment(x, "NonExisting"))
  expect_true(startsWith(getFromComment(x, "origin"),
                         "readSource(type = \"MetadataTest\", convert = \"onlycorrect\") (madrat "))
  expect_true(endsWith(getFromComment(x, "origin"), " | .GlobalEnv)"))
})


test_that("read functions can return non-magpie objects", {
  testReadSource <- function(readThis,
                             correctThis = function(x) readThis(),
                             convertThis = function(x) readThis(),
                             convert = TRUE,
                             supplementary = FALSE) {
    downloadThis <- function() list(url = "", author = "", title = "", license = "", description = "", unit = "")
    localConfig(globalenv = TRUE)
    stopifnot(!"This" %in% getCalculations(c("download", "read", "correct", "convert"))$type)
    globalassign("downloadThis", "readThis", "correctThis", "convertThis")
    return(readSource("This", convert = convert, supplementary = supplementary))
  }

  expect_identical(nce(testReadSource(function() list(x = 1, class = "numeric"), supplementary = TRUE)),
                   list(x = 1, class = "numeric", package = ".GlobalEnv"))

  # running second time -> loading from cache, will have additional attribute
  expect_false(identical(testReadSource(function() list(x = 1, class = "numeric"), supplementary = TRUE),
                         list(x = 1, class = "numeric", package = ".GlobalEnv")))
  expect_identical(nce(testReadSource(function() list(x = 1, class = "numeric"), supplementary = TRUE)),
                   list(x = 1, class = "numeric", package = ".GlobalEnv"))

  expect_identical(nce(testReadSource(function() list(x = 1, class = "numeric"))), 1)
  expect_error(testReadSource(function() list(x = 1, class = "character")),
               "Output of \"readThis()\" should have class \"character\" but it does not.",
               fixed = TRUE)
  expect_error(testReadSource(readThis = function() list(x = 1, class = "numeric"),
                              correctThis = function() list(x = 1, class = "character")),
               "Output of \"correctThis()\" should have class \"character\" but it does not.",
               fixed = TRUE)

  convertThis <- function(x) {
    expect_identical(nce(x), 1)
    return(list(x = 2, class = "numeric"))
  }
  expect_identical(nce(testReadSource(readThis = function() list(x = 1, class = "numeric"),
                                      convertThis = convertThis)), 2)
  expect_error(testReadSource(readThis = function() list(x = 1, class = "numeric"),
                              correctThis = function() list(x = 1, class = "numeric"),
                              convertThis = function() list(x = 1, class = "character")),
               "Output of \"convertThis()\" should have class \"character\" but it does not.",
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
  expect_identical(nce(testReadSource(function() list(x = brokenMagpie, class = "magpie"), convert = FALSE)),
                   clean_magpie(brokenMagpie))
})

test_that("readSource uses default subtype", {
  mainfolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  localConfig(mainfolder = mainfolder)

  downloadTest <- function(subtype = "a") {
    writeLines(subtype, "data.txt")
    return(list(url = "", author = "", title = "", license = "", description = "", unit = ""))
  }
  readTest <- function(subtype = "a") {
    stopifnot(file.exists("data.txt"))
    return(as.magpie(1))
  }
  globalassign(c("downloadTest", "readTest"))

  expect_false(file.exists(file.path(mainfolder, "sources", "Test", "a", "data.txt")))
  readSource("Test") # call readSource without passing subtype explicitly
  expect_true(file.exists(file.path(mainfolder, "sources", "Test", "a", "data.txt")))
})

test_that("read with subtype, download without", {
  mainfolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  localConfig(mainfolder = mainfolder)

  downloadTest <- function() {
    writeLines("z", "data.txt")
    return(list(url = "", author = "", title = "", license = "", description = "", unit = ""))
  }
  readTest <- function(subtype = "a") {
    stopifnot(file.exists("data.txt"))
    return(as.magpie(1))
  }
  globalassign(c("downloadTest", "readTest"))

  expect_false(file.exists(file.path(mainfolder, "sources", "Test", "data.txt")))
  readSource("Test") # call readSource without passing subtype explicitly
  expect_true(file.exists(file.path(mainfolder, "sources", "Test", "data.txt")))
})
