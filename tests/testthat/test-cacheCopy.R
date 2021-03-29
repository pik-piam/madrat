context("cacheCopy")

log <- c('Current madrat configuration:',
'   cachefolder      -> "/my/cache/folder"',
'   debug            -> FALSE',
'',
'Run retrieveData("Example", rev = 98.5, cachetype = "def")',
'',
'Run calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4")',
'> Run readSource("Tau", source)',
'>>  - writing cache readTau-Fe6e25820-f079dd0d.rds',
'>>  - writing cache convertTau-Fe6e25820-f079dd0d.rds',
'> Exit readSource("Tau", source) in 2.99 seconds',
'>  - writing cache calcTauTotal-F933da7d1.rds',
'Exit calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4") in 3.25 seconds',
'Exit retrieveData("Example", rev = 98.5, cachetype = "def") in 3.25 seconds')

test_that("cacheCopy properly detects cache files", {
  files <- c("/my/cache/folder/readTau-Fe6e25820-f079dd0d.rds", "/my/cache/folder/convertTau-Fe6e25820-f079dd0d.rds", 
             "/my/cache/folder/calcTauTotal-F933da7d1.rds")
  expect_identical(cacheCopy(log), files)
  expect_identical(cacheCopy(log, filter = "calc"), files[3])
  expect_warning(x <- cacheCopy(log, target=paste0(tempdir(),"/cachetarget")), "cache files could not be found")
  expect_identical(x,character(0))
  setConfig(cachefolder = )
  expect_warning(x <- cacheCopy(log[-2]), "No information about cachefolder found")
  expect_identical(basename(x), basename(files))
  expect_warning(x <- cacheCopy(c('cachefolder -> /bla/blub',log)), "More than one cache folder found")
  expect_identical(x,files)
})