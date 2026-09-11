test_that("getCode works", {
 localConfig(verbosity = 1, .verbose = FALSE)
 expect_silent({
   a <- getCode("madrat")
  })
 flags <- list(pucArguments = list(`madrat:::fullEXAMPLE` = "extra"),
               monitor = list(`madrat:::readTau` = c("madrat:::sysdata$iso_cell",
                                                     "magclass:::ncells")),
               ignore = list(`madrat:::readTau` = "madrat:::toolAggregate"))
 expect_identical(attr(a, "flags"), flags)
 expect_setequal(names(attributes(a)), c("names", "fpool", "hash", "mappings", "flags"))
 calcTauTotal <- function() {
   return(1)
 }
 calcFlagTest <- function() {
   "!# @ignore  testIgnore"
   "!# @ignore  ignoreMore"
   return(1)
 }
 globalassign("calcTauTotal", "calcFlagTest")
 expect_warning(a <- getCode("madrat"), "Duplicate functions")
 expect_setequal(attr(a, "flags")$ignore$calcFlagTest, c("testIgnore", "ignoreMore"))
 rm(list = c("calcTauTotal", "calcFlagTest"), envir = .GlobalEnv)
 expect_null(attr(getCode(NULL, TRUE), "flags"))
})

test_that("getCode resolves mapping names that are already absolute paths", {
  localConfig(verbosity = 1, .verbose = FALSE)
  dir.create(getConfig("mappingfolder"), recursive = TRUE, showWarnings = FALSE)

  # mirrors mrmagpie:::toolApplyRegionNames, which looks up getConfig("regionmapping")
  # via where = "mappingfolder" even though setConfig may have normalized it to an
  # absolute path already (e.g. when calcOutput's regionmapping argument points at a
  # file outside the mappingfolder)
  toolRegionNamesTest <- function() {
    toolGetMapping(type = "regional", where = "mappingfolder", name = getConfig("regionmapping"))
  }
  globalassign("toolRegionNamesTest")

  mappingPath <- file.path(getConfig("mappingfolder"), "regionmappingTest.csv")
  writeLines("CountryCode,RegionCode\nDEU,EUR", mappingPath)
  localConfig(regionmapping = mappingPath, .verbose = FALSE)

  expect_silent(a <- getCode("madrat"))
  expected <- toolGetMapping(type = "regional", where = "mappingfolder",
                             name = getConfig("regionmapping"), returnPathOnly = TRUE)
  expect_identical(attr(a, "mappings")[["toolRegionNamesTest"]], expected)
})
