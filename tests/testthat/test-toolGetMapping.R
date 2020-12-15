context("toolGetMapping")

test_that("toolGetMapping works", {
  expected <- structure(list(X = c("Aruba", "Afghanistan", "Angola", "Anguilla",
                                   "Aland Islands", "Albania"), 
                             CountryCode = c("ABW", "AFG", "AGO","AIA", "ALA", "ALB"), 
                             RegionCode = c("LAM", "OAS", "SSA", "LAM","EUR", "NEU")), 
                        row.names = c(NA, 6L), class = "data.frame")
  # find a mapping file in the current package that is not found also 
  # in the obvious place getConfig(mappingfolder)
  fname <- setdiff(grep("csv",dir(paste0(system.file(package = "madrat"),"/extdata")),value=TRUE),
                   grep("csv",dir(as.character(getConfig("mappingfolder"))),value=TRUE))[1]
  full_path <- paste0(system.file(package = "madrat"),"/extdata/",fname)
  # try to find that mapping as if you did not know where it is
  expect_identical(toolGetMapping(fname), read.csv(full_path,header = TRUE,sep = ";",colClasses = "character"))
  expect_identical(head(toolGetMapping("regionmappingH12.csv", where="madrat")), expected)
  expect_identical(toolGetMapping("bla",where="local", returnPathOnly = TRUE), "bla")
  expect_identical(toolGetMapping("/bla",where="local", returnPathOnly = TRUE), "/bla")
  expect_identical(toolGetMapping("/bla",type=".",where="local", returnPathOnly = TRUE), "./bla")
  expect_identical(head(toolGetMapping("regionmappingH12.csv")), expected)
})
