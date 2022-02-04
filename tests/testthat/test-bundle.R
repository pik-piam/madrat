test_that("bundle creation works", {
   skip_on_cran()
   skip_if_offline("zenodo.org")
   setConfig(mainfolder = withr::local_tempdir(), .verbose = FALSE, .local = TRUE)
   retrieveData("example", rev = 42, extra = "test1")
   expect_true(dir.exists(getConfig("bundlefolder")))
   withr::local_dir(getConfig("bundlefolder"))
   expect_true(file.exists("rev42_extra_example.bdl"))
   untar("rev42_extra_example.bdl")
   expect_true(file.exists("calcTauTotal.rds"))
   cfg <- readRDS("config.rds")
   expect_identical(cfg$package, "madrat")
   expect_identical(cfg$bundleArguments, "extra")
   expect_identical(cfg$args, list(model = "example", rev = 42, dev = "", cachetype = "rev",
                                   bundle = TRUE, extra = "test1"))
   expect_error(bundleCompile("rev42_extra_example.bdl", bla = "blub"), "cannot be changed in the given bundle")
   expect_message(bundleCompile("rev42_extra_example.bdl", extra = "blub", regionmapping = "regionmappingH12.csv"),
                  "Run calcOutput")
   expect_message(bundleCompile("rev42_extra_example.bdl", extra = "blub", regionmapping = "regionmappingH12.csv"),
                  "already available")
   expect_true(file.exists(file.path(getConfig("outputfolder"), "rev42_h12_7a5441e5_example_customizable_tag.tgz")))
   expect_message(retrieveData("example", rev = 42, extra = "test2"), "Run bundleCompile")
   expect_true(file.exists(file.path(getConfig("outputfolder"), "rev42_h12_5f3d77a0_example_customizable_tag.tgz")))
})
