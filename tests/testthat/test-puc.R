test_that("puc creation works", {
  skip_on_cran()
  retrieveData("example", rev = 42, extra = "test1")
  expect_true(dir.exists(getConfig("pucfolder")))
  withr::local_dir(getConfig("pucfolder"))
  expect_true(file.exists("rev42_extra_example_tag.puc"))
  untar("rev42_extra_example_tag.puc")
  expect_true(length(Sys.glob("calcTauTotal*.rds")) == 1)
  cfg <- readRDS("config.rds")
  expect_identical(cfg$package, "madrat")
  expect_identical(cfg$pucArguments, "extra")
  expect_identical(cfg$args, list(model = "example", rev = 42, dev = "", cachetype = "def",
                                  puc = TRUE, strict = FALSE, extra = "test1"))
  expect_error(pucAggregate("rev42_extra_example_tag.puc", bla = "blub"), "cannot be changed in the given puc")
  expect_message(pucAggregate("rev42_extra_example_tag.puc", extra = "blub", regionmapping = "regionmappingH12.csv",
                              renv = FALSE), "Run calcOutput")
  expect_message(pucAggregate("rev42_extra_example_tag.puc", extra = "blub", regionmapping = "regionmappingH12.csv",
                              renv = FALSE), "already available")
  expect_true(file.exists(file.path(getConfig("outputfolder"), "rev42_h12_7a5441e5_example_customizable_tag.tgz")))
  expect_message(retrieveData("example", rev = 42, extra = "test2", renv = FALSE), "Run pucAggregate")
  expect_true(file.exists(file.path(getConfig("outputfolder"), "rev42_h12_5f3d77a0_example_customizable_tag.tgz")))
})
