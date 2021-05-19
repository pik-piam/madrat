context("Data temporal averaging")

p <- magclass::maxample("pop")

test_that("Equivalence of data", {
  getCells(p) <- paste(getItems(p, dim=1),sample(c(1:length(getItems(p, dim=1)))), sep=".") 
  expect_equivalent(toolOrderCells(p)[getCells(p),,],p)
})

test_that("Ordering works properly for trivial case", {
  getCells(p) <- paste(getItems(p, dim=1),sample(c(1:length(getItems(p, dim=1)))), sep=".") 
  expect_equal(as.numeric(substring(getCells(toolOrderCells(p)),5)), 
               robustSort(as.numeric(substring(getCells(p),5))))
  expect_error(toolOrderCells(1), "Input is not a MAgPIE object")
})