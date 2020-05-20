context("readSource wrapper")

cfg <- getConfig(verbose = FALSE)

globalassign <- function(...) {
  for(x in c(...)) assign(x,eval.parent(parse(text=x)),.GlobalEnv)
}

test_that("readSource detects common problems", {
  setConfig(globalenv = TRUE, .verbose = FALSE, mainfolder=tempdir())
  readNoDownload <- function(){}
  globalassign("readNoDownload")
  expect_error(readSource("NoDownload"), "no download script")
  expect_error(readSource("Tau", subtype="historical", convert="WTF"), "Unknown convert setting")
  
  downloadTest <- function()return(1)
  readTest <- function()return(1)
  globalassign("downloadTest", "readTest")
  expect_error(readSource("Test"), "not a MAgPIE object")
  readTest <- function(x)return(as.magpie(1))
  globalassign("readTest")
  expect_warning(readSource("Test"),"Some arguments .* cannot be adressed by the wrapper")
  readTest <- function()return(as.magpie(1))
  convertTest <- function(x)return(as.magpie(1))
  globalassign("readTest","convertTest")
  expect_error(readSource("Test"),"Wrong number of countries")
  expect_warning(readSource("Test",convert = "onlycorrect"), "No correct function .* could be found")
})

