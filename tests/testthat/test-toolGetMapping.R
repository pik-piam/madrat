test_that("toolGetMapping works", {
  expected <- structure(list(X = c("Aruba", "Afghanistan", "Angola", "Anguilla",
                                   "Aland Islands", "Albania"),
                             CountryCode = c("ABW", "AFG", "AGO", "AIA", "ALA", "ALB"),
                             RegionCode = c("LAM", "OAS", "SSA", "LAM", "EUR", "NEU")),
                        row.names = c(NA, 6L), class = "data.frame")
  # find a mapping file in the current package that is not found also
  # in the obvious place getConfig("mappingfolder")
  fname <- setdiff(grep("csv", dir(paste0(system.file(package = "madrat"), "/extdata")), value = TRUE),
                   grep("csv", dir(as.character(getConfig("mappingfolder"))), value = TRUE))[1]
  fullPath <- paste0(system.file(package = "madrat"), "/extdata/", fname)
  # try to find that mapping as if you did not know where it is
  expect_identical(toolGetMapping(fname), read.csv(fullPath, header = TRUE, sep = ";", colClasses = "character"))
  expect_identical(toolGetMapping(fullPath, type = "csv"), toolGetMapping(fname))
  expect_identical(head(toolGetMapping("regionmappingH12.csv", where = "madrat")), expected)
  expect_identical(toolGetMapping("bla", where = "local", returnPathOnly = TRUE, error.missing = FALSE), "bla")
  expect_identical(toolGetMapping("/bla", where = "local", returnPathOnly = TRUE, error.missing = FALSE), "/bla")
  expect_identical(toolGetMapping("/bla", type = ".", where = "local", returnPathOnly = TRUE, error.missing = FALSE),
                   "./bla")
  expect_identical(head(toolGetMapping("regionmappingH12.csv")), expected)

  expect_error(toolGetMapping("notexisting.csv"), "not found!")
  expect_error(toolGetMapping("notexisting.csv", where = "mappingfolder"), "not found")
  expect_error(toolGetMapping("notexisting.csv", where = "local"), "not found")
  expect_error(toolGetMapping("notexisting.csv", where = "madrat"), "not found")

  localConfig(.verbose = FALSE)
  dir.create(getConfig("mappingfolder"), showWarnings = FALSE)
  expect_silent(toolGetMapping("regionmappingH12.csv"))
  expect_error(toolGetMapping("regionmappingH12.csv", where = "mappingfolder"), "not found!")

  write.table(toolGetMapping("regionmappingH12.csv"), paste0(getConfig("mappingfolder"), "/test.csv"), sep = ",",
              row.names = FALSE)
  expect_identical(toolGetMapping("test.csv", where = "mappingfolder"), toolGetMapping("regionmappingH12.csv"))

  a <- 1
  save(a, file = paste0(getConfig("mappingfolder"), "/test.rda"))
  writeLines("abc", paste0(getConfig("mappingfolder"), "/test.xyz"))
  expect_error(toolGetMapping("test.rda", where = "mappingfolder"), "did not contain an object")
  expect_error(toolGetMapping("test.xyz"), "Unsupported filetype")

  readTest <- function() {
    toolGetMapping("regionmappingH12.csv", type = "regional")
    return(as.magpie(1))
  }
  globalassign("readTest")
  expect_warning(readSource("Test"),
                 "argument 'where' should be set when calling toolGetMapping from within a madrat function.")
})
