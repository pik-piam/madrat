
test_that("wrapper activity setting and detection works", {

  pseudoCalc <- function() {
    setWrapperActive("calcOutput")
    expect_false(isWrapperActive("readSource"))
    expect_true(isWrapperActive("calcOutput"))
  }

  expect_false(isWrapperActive("readSource"))
  expect_false(isWrapperActive("calcOutput"))

  pseudoCalc()

  expect_false(isWrapperActive("readSource"))
  expect_false(isWrapperActive("calcOutput"))

  pseudoRead <- function() {
    setWrapperActive("readSource")
    return(c(isWrapperActive("readSource"), isWrapperActive("calcOutput")))
  }

  expect_identical(pseudoRead(), c(TRUE, FALSE))

  pseudoCalc2 <- function() {
    setWrapperActive("calcOutput")
    expect_false(isWrapperActive("readSource"))
    expect_true(isWrapperActive("calcOutput"))
    expect_identical(pseudoRead(), c(TRUE, TRUE))
    expect_false(isWrapperActive("readSource"))
    expect_true(isWrapperActive("calcOutput"))
  }
  pseudoCalc2()

  pseudoCalc3 <- function() {
    setWrapperActive("calcOutput")
    expect_true(isWrapperActive("calcOutput"))
    setWrapperInactive("calcOutput")
    expect_false(isWrapperActive("calcOutput"))
  }
  pseudoCalc3()

  expect_error(setWrapperActive("blablub"), "Unknown wrapper")
  expect_error(isWrapperActive("blablub"), "Unknown wrapper")
})
