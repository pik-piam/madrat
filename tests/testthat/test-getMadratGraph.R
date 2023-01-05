test_that("getMadratGraph works", {
  calcTest1 <- function() return(1)
  calcTest2 <- function() return(calcOutput("Test1"))
  globalassign("calcTest1", "calcTest2")
  expect_silent({
    graph <- getMadratGraph(packages = "madrat")
  })
  expect_setequal(graph$from_package, c("madrat", ".GlobalEnv"))
  expect_setequal(graph$to_package, c("madrat", ".GlobalEnv"))
})

test_that("MADRaT Universe detection works", {
  expect_true("madrat" %in% installedMadratUniverse())
})
