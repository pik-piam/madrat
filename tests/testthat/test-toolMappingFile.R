context("toolMappingFile")

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
  expect_identical(suppressWarnings(toolMappingFile("",fname, readcsv = TRUE)), read.csv(full_path,header = TRUE,sep = ";",colClasses = "character"))
  expect_identical(head(suppressWarnings((toolMappingFile("","regionmappingH12.csv", where="madrat", readcsv = TRUE)))), expected)
  
  expect_error(suppressWarnings(toolMappingFile("","notexisting.csv")), "not found!")
  expect_error(suppressWarnings(toolMappingFile("","notexisting.csv", where = "mappingfolder")), "not found")
  expect_error(suppressWarnings(toolMappingFile("","notexisting.csv", where = "local")), "not found")
  expect_error(suppressWarnings(toolMappingFile("","notexisting.csv", where = "madrat")), "not found")
})
