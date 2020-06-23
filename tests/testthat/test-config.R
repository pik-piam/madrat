context("MADRaT configuration")

cfg <- getConfig(verbose = FALSE)

test_that("cache folder can be set properly", {
  setConfig(cachefolder=getwd(), .verbose = FALSE)
  expect_identical(getwd(),getConfig("cachefolder"))
  
  # test shortcut formulation
  setConfig(cachefolder="rev123", .verbose = FALSE)
  expect_identical(file.path(getConfig("mainfolder"),"cache/rev123"),getConfig("cachefolder"))
})

