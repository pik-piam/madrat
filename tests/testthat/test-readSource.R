cfg <- getConfig(verbose = FALSE)

globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
}

nce <- function(x) {
  getComment(x) <- NULL
  attr(x, "cachefile") <- NULL
  attr(x, "id") <- NULL
  return(x)
}

test_that("readSource waits until download is finished", {
  mainfolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  setConfig(mainfolder = mainfolder, .local = TRUE)
  dir.create(file.path(mainfolder, "sources", "Tau-downloadInProgress"), recursive = TRUE)
  expect_error(readSource("Tau", numberOfTries = 1), "The download did not finish in time.", fixed = TRUE)
  dir.create(file.path(mainfolder, "sources", "Tau"), recursive = TRUE)
  expect_error(readSource("Tau", numberOfTries = 1.45),
               "as.integer(numberOfTries) == numberOfTries is not TRUE", fixed = TRUE)
  expect_error(readSource("Tau", numberOfTries = 1:3), "length(numberOfTries) == 1 is not TRUE", fixed = TRUE)
  expect_error(readSource("Tau", numberOfTries = -1), "numberOfTries >= 1 is not TRUE", fixed = TRUE)
  withr::with_options(list(warn = 2), {# turn warning into error so execution is stopped after warning
    expect_error(readSource("Tau", numberOfTries = 1),
                 paste0("The folders ", file.path(mainfolder, "sources", "Tau"), " and ",
                        file.path(mainfolder, "sources", "Tau-downloadInProgress"),
                        " should not exist at the same time."),
                 fixed = TRUE)
  })
})

test_that("readSource detects common problems", {
  setConfig(globalenv = TRUE, verbosity = 2, .verbose = FALSE, mainfolder = tempdir(), .local = TRUE)
  readNoDownload <- function() {} # nolint
  globalassign("readNoDownload")
  expect_error(readSource("NoDownload"), "no download script")

  downloadTest <- function() {
    return(list(url = "dummy", author = "dummy", title = "dummy", license = "dummy",
                description = "dummy", unit = "dummy"))
  }
  readTest <- function() return(1)
  globalassign("downloadTest", "readTest")
  expect_error(readSource("Test"), "not a MAgPIE object")
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
  setConfig(globalenv = TRUE, verbosity = 2, .verbose = FALSE, mainfolder = tempdir(), .local = TRUE)
  expect_error(downloadSource("Tau", "paper"), "does already exist!")
  expect_error(downloadSource(1:10), "Invalid type")
  expect_error(downloadSource("Tau", subtype = 1:10), "Invalid subtype")
  downloadTest <- function() return(list(url = 1, author = 1, title = 1, license = 1,
                                         description = 1, unit = 1, call = "notallowed"))
  globalassign("downloadTest")
  expect_warning(downloadSource("Test", overwrite = TRUE), "reserved and will be overwritten")
})

rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
