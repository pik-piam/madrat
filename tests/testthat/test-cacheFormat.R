# NOTE: the calc functions defined in the tests below must all have distinct bodies. The
# madrat graph is cached (see getMadratGraph) under a hash which does not distinguish
# identically defined functions, so reusing a body makes a later test look up a function
# which is not part of the cached graph.

# register a format which is distinguishable from rds, so tests can prove which reader ran
localTestFormat <- function(name = "testformat", extension = "tf", toRds = NULL,
                            .localEnvir = parent.frame()) {
  # snapshot the registry so repeated/nested registrations restore correctly
  withr::local_options(madrat_cacheformats = getOption("madrat_cacheformats"),
                       .local_envir = .localEnvir)
  registerCacheFormat(name, extension = extension, toRds = toRds,
                      write = function(x, file) saveRDS(list(testformat = TRUE, payload = x), file),
                      read = function(file) {
                        content <- readRDS(file)
                        stopifnot(isTRUE(content$testformat))
                        return(content$payload)
                      })
}

# register a format whose declared package is not installed, so tests can prove
# checkCacheFormatAvailable() catches it
localFormatWithMissingPackage <- function(name = "needspackage", extension = "np",
                                          .localEnvir = parent.frame()) {
  withr::local_options(madrat_cacheformats = getOption("madrat_cacheformats"),
                       .local_envir = .localEnvir)
  noop <- function(x, file) NULL
  registerCacheFormat(name, extension = extension, write = noop, read = noop,
                      packages = "thisPackageIsNotInstalled")
}

test_that("built-in cache formats are available and rds is the default", {
  expect_true(all(c("rds", "qs2") %in% cacheFormats()))
  expect_identical(cacheFormat("rds")$extension, "rds")
  expect_identical(cacheFormat("rds")$name, "rds")

  skip_if(nzchar(Sys.getenv("MADRAT_CACHEFORMAT")), "MADRAT_CACHEFORMAT overrides the default")
  expect_identical(getConfig("cacheformat"), "rds")

  # a config written by a madrat version which did not know cacheformat must still work
  cfg <- getOption("madrat_cfg")
  cfg$cacheformat <- NULL
  withr::local_options(madrat_cfg = cfg)
  expect_identical(getConfig("cacheformat"), "rds")
  expect_identical(cacheFormat()$extension, "rds")
})

test_that("registerCacheFormat validates its input", {
  noop <- function(x, file) NULL
  withr::local_options(madrat_cacheformats = getOption("madrat_cacheformats"))
  expect_error(registerCacheFormat("a", write = noop, read = noop, extension = "a-b"),
               "must only contain alphanumeric characters")
  expect_error(registerCacheFormat("a", write = noop, read = noop, extension = "a.b"),
               "must only contain alphanumeric characters")
  # an extension may not be claimed by two different formats
  expect_error(registerCacheFormat("myrds", write = noop, read = noop, extension = "rds"),
               "already used by another cache format")
  # but re-registering the same name is fine
  expect_silent(registerCacheFormat("a", write = noop, read = noop, extension = "aa"))
  expect_silent(registerCacheFormat("a", write = noop, read = noop, extension = "aa"))
  expect_true("a" %in% cacheFormats())
})

test_that("setConfig rejects a cacheformat which is not registered", {
  before <- getConfig("cacheformat")
  expect_error(setConfig(cacheformat = "doesnotexist", .verbose = FALSE), "Unknown cache format")
  expect_error(setConfig(cacheformat = 42, .verbose = FALSE), "Unknown cache format")
  expect_error(setConfig(cacheformat = c("rds", "rds"), .verbose = FALSE), "Unknown cache format")
  # unchanged after all those failures
  expect_identical(getConfig("cacheformat"), before)
})

test_that("setConfig rejects a cacheformat whose declared package is not installed", {
  localFormatWithMissingPackage()

  before <- getConfig("cacheformat")
  expect_error(setConfig(cacheformat = "needspackage", .verbose = FALSE),
               "needspackage.*thisPackageIsNotInstalled.*not installed")
  expect_identical(getConfig("cacheformat"), before)

  # .cfgchecks = FALSE bypasses the check, same as it bypasses the "unknown format" check
  expect_silent(setConfig(cacheformat = "needspackage", .cfgchecks = FALSE, .verbose = FALSE))
  expect_identical(getConfig("cacheformat"), "needspackage")

  # a format without declared packages (like the builtin "rds") is unaffected
  setConfig(cacheformat = before, .verbose = FALSE)
  expect_silent(setConfig(cacheformat = "rds", .verbose = FALSE))
})

test_that("initializeConfig rejects MADRAT_CACHEFORMAT when its package is not installed", {
  localFormatWithMissingPackage()

  withr::local_options(madrat_cfg = NULL)
  withr::local_envvar(MADRAT_CACHEFORMAT = "needspackage")
  expect_error(initializeConfig(verbose = FALSE),
               "needspackage.*thisPackageIsNotInstalled.*not installed.*MADRAT_CACHEFORMAT")
})

test_that("a cache format which cannot be used fails softly", {
  withr::local_options(madrat_cacheformats = getOption("madrat_cacheformats"))
  # simulates a format depending on an uninstalled package, without actually writing a
  # `pkg::fun` reference, which the deps-in-desc pre-commit hook would flag as a missing
  # dependency
  missingPackageError <- function(...) stop("there is no package called 'thisPackageIsNotInstalled'")
  registerCacheFormat("brokenformat", extension = "bf",
                      write = missingPackageError, read = missingPackageError)
  # configuring a format is allowed even if it cannot be used, caching is optional
  localConfig(cachefolder = withr::local_tempdir(), outputfolder = withr::local_tempdir(),
              cacheformat = "brokenformat", .verbose = FALSE)
  calcBrokenFormatExample <- function() return(list(x = as.magpie(11), description = "-", unit = "-"))
  globalassign("calcBrokenFormatExample")

  # the calculation still returns its result, only the caching fails, stating the reason
  setWrapperActive("saveCache")
  expect_warning(x <- calcOutput("BrokenFormatExample", aggregate = FALSE),
                 "could not write cache file.*thisPackageIsNotInstalled")
  expect_identical(as.vector(x), 11)
  expect_length(cacheGlob(), 0)
  # and the cache file which was never written must not be listed for the puc
  expect_false(file.exists(file.path(getConfig("outputfolder"), "pucFiles")))
})

test_that("cache files are written in the configured format", {
  localTestFormat()
  localConfig(cachefolder = withr::local_tempdir(), cacheformat = "testformat", .verbose = FALSE)
  calcCacheFormatExample <- function() return(list(x = as.magpie(1), description = "-", unit = "-"))
  globalassign("calcCacheFormatExample")

  expect_message(calcOutput("CacheFormatExample", aggregate = FALSE), "writing cache")
  written <- cacheGlob()
  expect_length(written, 1)
  expect_match(written, "calcCacheFormatExample-F[^-]*\\.tf$")
  # the custom writer really ran
  expect_true(readRDS(written)$testformat)

  # and it is read back through the matching reader
  expect_message(x <- calcOutput("CacheFormatExample", aggregate = FALSE), "loading cache.*\\.tf")
  expect_identical(as.vector(x), 1)
})

test_that("an existing rds cache file is used as read-only fallback", {
  localTestFormat()
  localConfig(cachefolder = withr::local_tempdir(), cacheformat = "rds", .verbose = FALSE)
  calcFallbackExample <- function() return(list(x = as.magpie(2), description = "-", unit = "-"))
  globalassign("calcFallbackExample")

  # populate the cache with rds, as it would exist before switching the format
  expect_message(calcOutput("FallbackExample", aggregate = FALSE), "writing cache")
  rdsFile <- cacheGlob("*.rds")
  expect_length(rdsFile, 1)

  localConfig(cacheformat = "testformat", .verbose = FALSE)
  cf <- cacheNames("calc", "FallbackExample")
  # the write target uses the configured format, the file to read is the existing rds
  expect_match(basename(cf$write), "\\.tf$")
  expect_identical(cf$read, rdsFile)

  # the rds file is read and NOT rewritten in the new format
  expect_message(x <- calcOutput("FallbackExample", aggregate = FALSE), "loading cache.*\\.rds")
  expect_identical(as.vector(x), 2)
  expect_length(cacheGlob("*.tf"), 0)
  expect_true(file.exists(rdsFile))
})

test_that("the configured format wins over rds for an identical fingerprint", {
  localTestFormat()
  localConfig(cachefolder = withr::local_tempdir(), cacheformat = "rds", .verbose = FALSE)
  calcPrecedenceExample <- function() return(list(x = as.magpie(3), description = "-", unit = "-"))
  globalassign("calcPrecedenceExample")

  localConfig(cacheformat = "testformat", .verbose = FALSE)
  calcOutput("PrecedenceExample", aggregate = FALSE)
  tfFile <- cacheGlob("*.tf")
  expect_length(tfFile, 1)

  # a newer rds file with the same fingerprint must not take precedence, as an identical
  # fingerprint implies identical content
  rdsFile <- sub("\\.tf$", ".rds", tfFile)
  Sys.sleep(1)
  saveRDS(list(x = as.magpie(99), class = "magpie"), rdsFile)
  expect_identical(cacheNames("calc", "PrecedenceExample")$read, tfFile)

  # with forcecache and differing fingerprints the newest file wins, across formats
  calcPrecedenceExample <- function() return(list(x = as.magpie(33), description = "-", unit = "-"))
  globalassign("calcPrecedenceExample")
  localConfig(forcecache = TRUE, .verbose = FALSE)
  expect_message(cf <- cacheNames("calc", "PrecedenceExample"), "does not match fingerprint")
  expect_identical(cf$read, rdsFile)
})

test_that("cache files with an args hash are not mistaken for ones without", {
  localTestFormat()
  localConfig(cachefolder = withr::local_tempdir(), forcecache = TRUE, .verbose = FALSE)
  calcArgsHashExample <- function(subtype = "a") return(list(x = as.magpie(4), description = "-", unit = "-"))
  globalassign("calcArgsHashExample")
  localConfig(cacheformat = "testformat", .verbose = FALSE)

  # a file carrying an args hash must not be picked up when no args hash is requested
  file.create(file.path(getConfig("cachefolder"), "calcArgsHashExample-Fabcdef01-12345678.tf"))
  expect_null(cacheNames("calc", "ArgsHashExample")$read)

  withoutArgs <- file.path(getConfig("cachefolder"), "calcArgsHashExample-Fabcdef01.tf")
  file.create(withoutArgs)
  expect_identical(cacheNames("calc", "ArgsHashExample")$read, withoutArgs)
})

test_that("cacheToRds converts via the format specific and the generic path", {
  target <- file.path(withr::local_tempdir(), "out.rds")
  payload <- list(x = as.magpie(1), class = "magpie")

  # rds -> rds is a plain copy
  cacheFile <- file.path(withr::local_tempdir(), "in.rds")
  saveRDS(payload, cacheFile)
  expect_true(cacheToRds(cacheFile, target))
  expect_identical(readRDS(target), payload)

  # a format specific toRds is used if given
  calls <- new.env(parent = emptyenv())
  calls$toRds <- 0
  localTestFormat(name = "withtords", extension = "wtr",
                  toRds = function(input, output) {
                    calls$toRds <- calls$toRds + 1
                    saveRDS(readRDS(input)$payload, output)
                  })
  cacheFile <- file.path(withr::local_tempdir(), "in.wtr")
  cacheWrite(payload, cacheFile)
  expect_true(cacheToRds(cacheFile, target))
  expect_identical(calls$toRds, 1)
  expect_identical(readRDS(target), payload)

  # without toRds the generic read-then-saveRDS path is used
  localTestFormat(name = "withouttords", extension = "wor", toRds = NULL)
  cacheFile <- file.path(withr::local_tempdir(), "in.wor")
  cacheWrite(payload, cacheFile)
  expect_true(cacheToRds(cacheFile, target))
  expect_identical(readRDS(target), payload)
})

test_that("puc files contain rds cache files regardless of the cache format", {
  localMockedTauDownload()
  localTestFormat()
  localConfig(cachefolder = withr::local_tempdir(), pucfolder = withr::local_tempdir(),
              outputfolder = withr::local_tempdir(), cacheformat = "testformat", .verbose = FALSE)

  retrieveData("example", rev = 43, extra = "test", renv = FALSE, puc = TRUE)

  # the cache itself uses the configured format
  expect_gt(length(cacheGlob("*.tf")), 0)

  puc <- Sys.glob(file.path(getConfig("pucfolder"), "*.puc"))
  expect_length(puc, 1)
  withr::with_tempdir({
    untar(puc)
    expect_length(Sys.glob("*.tf"), 0)
    cacheFiles <- Sys.glob("calc*.rds")
    expect_gt(length(cacheFiles), 0)
    # readable without any knowledge of the cache format used during processing
    for (cacheFile in cacheFiles) expect_true(is.list(readRDS(cacheFile)))
  })

  # and the puc can be aggregated while a non-rds format is still configured
  expect_message(pucAggregate(puc, regionmapping = "regionmappingH12.csv", renv = FALSE),
                 "load data from puc|Run calcOutput|retrieveData")
})

test_that("puc creation works when all cache files come from the rds fallback", {
  localMockedTauDownload()
  # regression test: the puc manifest used to record the file which would be written
  # rather than the one which exists, so a puc was silently skipped in this case
  localConfig(cachefolder = withr::local_tempdir(), pucfolder = withr::local_tempdir(),
              outputfolder = withr::local_tempdir(), cacheformat = "rds", .verbose = FALSE)

  # fill the cache using rds
  retrieveData("example", rev = 44, extra = "test", renv = FALSE, puc = FALSE)
  expect_gt(length(cacheGlob("*.rds")), 0)

  # now switch the format, so every calculation is served from the rds fallback
  localTestFormat()
  # a fresh outputfolder, as retrieveData would otherwise just reuse the existing tgz
  localConfig(cacheformat = "testformat", outputfolder = withr::local_tempdir(), .verbose = FALSE)
  retrieveData("example", rev = 44, extra = "test", renv = FALSE, puc = TRUE)

  puc <- Sys.glob(file.path(getConfig("pucfolder"), "*.puc"))
  expect_length(puc, 1)
  withr::with_tempdir({
    untar(puc)
    expect_gt(length(Sys.glob("calc*.rds")), 0)
  })
})

test_that("the qs2 cache format works end to end", {
  skip_if_not_installed("qs2")
  localConfig(cachefolder = withr::local_tempdir(), cacheformat = "qs2", .verbose = FALSE)
  calcQs2Example <- function() return(list(x = as.magpie(1:3), description = "-", unit = "-"))
  globalassign("calcQs2Example")

  expect_message(calcOutput("Qs2Example", aggregate = FALSE), "writing cache")
  qs2File <- cacheGlob("*.qs2")
  expect_length(qs2File, 1)
  # a genuine qs2 file, not an rds file with a different name
  expect_error(readRDS(qs2File), NULL)
  expect_true(is.list(qs2::qs_read(qs2File)))

  expect_message(x <- calcOutput("Qs2Example", aggregate = FALSE), "loading cache.*\\.qs2")
  expect_identical(as.vector(x), 1:3)

  # qs_to_rds produces a file which plain readRDS understands, including attributes
  rdsFile <- file.path(withr::local_tempdir(), "converted.rds")
  expect_true(cacheToRds(qs2File, rdsFile))
  expect_identical(readRDS(rdsFile), qs2::qs_read(qs2File))
})

test_that("file based terra objects can be cached in another format", {
  skip_if_not_installed("terra")
  localTestFormat()
  localConfig(cachefolder = withr::local_tempdir(), cacheformat = "testformat", .verbose = FALSE)

  downloadTerraFormat <- function() {
    return(list(url = 0, author = 0, title = 0, license = 0, description = 0, unit = 0))
  }
  readTerraFormat <- function() {
    x <- terra::rast(system.file("ex/meuse.tif", package = "terra"))
    names(x) <- "terraFormatLayer"
    return(list(x = x, class = "SpatRaster"))
  }
  globalassign("downloadTerraFormat", "readTerraFormat")

  expect_message(a <- readSource("TerraFormat"), "writing cache")
  expect_message(b <- readSource("TerraFormat"), "loading cache.*\\.tf")
  # the cache file itself uses the configured format, while the raster source file copied
  # next to it keeps its own extension
  expect_length(cacheGlob("readTerraFormat-F*.tf"), 1)
  expect_length(cacheGlob("readTerraFormat-F*-x.tif"), 1)
  expect_equal(terra::as.data.frame(a, xy = TRUE), terra::as.data.frame(b, xy = TRUE))
  expect_identical(names(a), names(b))
})
