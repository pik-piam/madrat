test_that("getMadratInfo works without error", {
  localConfig(globalenv = FALSE, .verbose = FALSE)
  expect_message(a <- getMadratInfo(packages = "madrat", cutoff = -1, extended = TRUE), "passed")
})

test_that("getMadratInfo properly detects problems", {
  calcBla <- function() {
    type <- "TauTotal"
    calcOutput(type)
  }
  globalassign("calcBla")
  expect_warning(a <- getMadratInfo(packages = "madrat", cutoff = 1),
                 "Following functions contain read or calc statements which could not be identified: .* calcBla")
  rm("calcBla", envir = .GlobalEnv)

  expect_silent(a <- suppressMessages(getMadratInfo(packages = "madrat")))
  toolBla <- function() {
    return(calcOutput("TauTotal"))
  }
  globalassign("toolBla")
  expect_warning(a <- getMadratInfo(packages = "madrat"), "Some tool functions contain read or calc")

  calcBla2 <- function() {
    calcOutput("UnknownType")
  }
  globalassign("calcBla2")
  expect_warning(a <- getMadratInfo(packages = "madrat"),
      "Following functions could not be found in the scope of packages to be checked.: .* calcUnknownType->calcBla2")
})


test_that("bidirectional package connections are correctly detected", {
  g <- data.frame(from = c("readData1", "calcExample", "calcExample"),
                  to = c("calcExample", "calcExample2", "calcExample3"),
                  from_package = c("pkgA", "pkgB", "pkgB"),
                  to_package = c("pkgB", "pkgA", "pkgA"),
                  stringsAsFactors = FALSE)

  attr(g, "fpool") <- data.frame(type = c("Data1", "Example", "Example2", "Example3"),
                                 package = c("pkgA", "pkgB", "pkgA", "pkgA"),
                                 call = c("pkgA:::readData1", "pkgB:::calcExample",
                                          "pkgA:::calcExample2", "pkgA:::calcExample3"),
                                 fname = c("readData1", "calcExample", "calcExample2", "calcExample2"),
                                 stringsAsFactors = FALSE)

  expect_warning(a <- getMadratInfo(g), "Bidirectional package dependencies detected")
  expect_warning(b <- getMadratInfo(g, cutoff = 1), "Bidirectional package dependencies detected")
  expect_identical(a, b)
})
