context("Data calculation wrapper")

cfg <- getConfig(verbose = FALSE)

test_that("calcOutput will stop if unused arguments are provided", {
  setConfig(globalenv = TRUE)
  calcTest <- function(testarg=FALSE) {
    return(list(x=as.magpie(0),
                weight=NULL,
                isocountries=FALSE,
                unit="1",
                description="calcOutput test data dummy"))
  }
  expect_error(calcOutput("Test",testarg=TRUE,blubba=1,aggregate = FALSE))
  setConfig(globalenv = cfg$globalenv)
})

