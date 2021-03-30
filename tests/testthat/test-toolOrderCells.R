context("Data temporal averaging")

data("population_magpie")

test_that("Equivalence of data", {
  p <- population_magpie
  getCells(p) <- paste(getItems(p, dim=1),sample(c(1:length(getItems(p, dim=1)))), sep=".") 
  expect_equivalent(toolOrderCells(p)[getCells(p),,],p)
})

test_that("Ordering works properly for trivial case", {
  p <- population_magpie
  getCells(p) <- paste(getItems(p, dim=1),sample(c(1:length(getItems(p, dim=1)))), sep=".") 
  expect_equal(as.numeric(substring(getCells(toolOrderCells(p)),5)), 
               sort(as.numeric(substring(getCells(p),5))))
  expect_error(toolOrderCells(1), "Input is not a MAgPIE object")
})