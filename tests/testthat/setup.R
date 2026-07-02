# set madrat mainfolder and options before running tests, cleanup after all tests are finished

# enable sourcing this file during interactive development of tests
teardownEnv <- tryCatch(testthat::teardown_env(), error = function(e) .GlobalEnv)

previousMadratConfig <- getOption("madrat_cfg")
withr::defer({
  options(madrat_cfg = previousMadratConfig) # nolint
}, envir = teardownEnv)

setConfig(mainfolder = withr::local_tempdir(.local_envir = teardownEnv),
          # Ensure that a folders in the tmp folder are used.
          cachefolder = "cache",
          pucfolder = withr::local_tempdir(.local_envir = teardownEnv),
          outputfolder = withr::local_tempdir(.local_envir = teardownEnv),
          globalenv = TRUE,
          redirections = list(),
          .verbose = FALSE)
withr::local_options(madrat_codelabels = NULL,
                     MadratCache = NULL,
                     renv.verbose = FALSE,
                     magclass_expand_version = NULL,
                     magclass_sizeLimit = NULL,
                     .local_envir = teardownEnv)

withr::local_envvar(LANGUAGE = "EN",
                    .local_envir = teardownEnv)

globalassign <- function(...) {
  withr::defer({
    # suppress warning if object does not exist in .GlobalEnv
    suppressWarnings(rm(list = c(...), envir = .GlobalEnv))
  }, envir = parent.frame())
  for (x in c(...)) {
    assign(x, eval.parent(parse(text = x)), .GlobalEnv)
  }
}

# Build small synthetic replacements for the Tau downloads once, so tests do not have to hit the
# network (zenodo.org). Each zip mimics what downloadTau/readTau expect for a subtype: the "paper"
# subtype reads tau_data_1995-2000.mz, the "historical" subtype reads tau_xref_history_country.mz.
# The "total" crop band is the one calcTauTotal reads (tau.total/xref.total). The fill-source
# countries (IDN, CHN, QAT) required by convertTau's toolCountryFill call must be present. Values are
# synthetic, so tests must not assert on exact Tau numbers, only on structure/years.
mockTauCountries <- c("DEU", "FRA", "USA", "IDN", "CHN", "QAT")

.buildMockTauZip <- function(mzName, years) {
  zipDir <- withr::local_tempdir(.local_envir = teardownEnv)
  m <- new.magpie(mockTauCountries, years, c("tau.total", "xref.total"), fill = 1)
  m[, , "xref.total"] <- 2
  write.magpie(m, file.path(zipDir, mzName))
  zipPath <- file.path(zipDir, "tau.zip")
  withr::with_dir(zipDir, utils::zip(zipPath, mzName, flags = "-q"))
  zipPath
}

mockTauZipPaper <- .buildMockTauZip("tau_data_1995-2000.mz", c(1995, 2000))
# historical subtype: yearly data 1970-2007 (38 years), 2008 absent - matches the calcOutput checks
mockTauZipHistorical <- .buildMockTauZip("tau_xref_history_country.mz", 1970:2007)

# Mock downloadTau's network request within a test: replace the download.file binding in the madrat
# namespace so it copies the appropriate synthetic zip (dispatched by URL) instead of downloading
# from zenodo.org.
localMockedTauDownload <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      zip <- if (any(grepl("historical", url))) mockTauZipHistorical else mockTauZipPaper
      file.copy(zip, destfile, overwrite = TRUE)
      0L
    },
    .package = "madrat",
    .env = env
  )
}
