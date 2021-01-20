context("MADRaT analysis tools")

cfg <- getConfig(verbose = FALSE)

globalassign <- function(...) {
  for(x in c(...)) assign(x,eval.parent(parse(text=x)),.GlobalEnv)
}

test_that("getMadratInfo works without error", {
  setConfig(.verbose = FALSE)
  expect_message(a <- getMadratInfo(packages=getConfig("packages")),"passed")
})

test_that("getMadratInfo properly detects problems", {
  setConfig(globalenv = TRUE, .verbose = FALSE)
  calcBla <- function() {type<-"TauTotal";calcOutput(type)}
  globalassign("calcBla")
  expect_warning(a <- getMadratInfo(packages=getConfig("packages")),"Following functions contain read or calc statements which could not be identified: .* calcBla")
  rm(calcBla,envir = .GlobalEnv)
  expect_silent(a <- suppressMessages(getMadratInfo(packages=getConfig("packages"))))
  toolBla <- function() {return(calcOutput("TauTotal"))}
  globalassign("toolBla")
  expect_message(a <- getMadratInfo(packages=getConfig("packages")), "[warning].*toolBla")
  rm(toolBla,envir = .GlobalEnv)
  calcBla2 <- function() {calcOutput("UnknownType")}
  globalassign("calcBla2")
  expect_warning(a <- getMadratInfo(packages=getConfig("packages")),"Following functions could not be found in the scope of packages to be checked.: .* calcUnknownType->calcBla2")
})