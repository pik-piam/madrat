context("Data temporal averaging")

data("population_magpie")

test_that("Proper detection of time step completness", {
  p <- population_magpie
  p[,,] <- 1
  getYears(p) <- 1:nyears(p)
  expect_equivalent(toolTimeAverage(p, averaging_range=1, cut=FALSE),p)
})

test_that("Averaging works properly for trivial case", {
  p <- population_magpie
  p[,,] <- 1
  getYears(p) <- 1900 + 1:nyears(p)
  expect_equivalent(toolTimeAverage(p, averaging_range=4, cut=FALSE),p)
})