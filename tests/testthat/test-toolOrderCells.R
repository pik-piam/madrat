p <- magclass::maxample("pop")

test_that("Equivalence of data", {
  getCells(p) <- paste(getItems(p, dim = 1), sample(seq_along(getItems(p, dim = 1))), sep = ".")
  expect_equivalent(toolOrderCells(p)[getCells(p), , ], p)
})

test_that("Ordering works properly for trivial case", {
  getCells(p) <- paste(getItems(p, dim = 1), sample(seq_along(getItems(p, dim = 1))), sep = ".")
  expect_equal(as.numeric(substring(getCells(toolOrderCells(p)), 5)),
               robustSort(as.numeric(substring(getCells(p), 5))))
  expect_error(toolOrderCells(1), "Input is not a MAgPIE object")
})

test_that("NA handling works", {
  a <- magclass::maxample("animal")
  getItems(a, dim = 1.4)
  getItems(a, dim = 1.4)[5:8] <- c("A", "B", "C", "D")
  expect_identical(a, toolOrderCells(a, dim = 1.4))
  out <- c("13987", "13988", "14003", "14004", "14018", "14019", "14020",
           "14021", "14035", "14036", "14037", "14038", "14040", "14053",
           "14054", "14055", "14056", "14057", "14058", "14077", "14078",
           "14079", "14080", "14081", "14082", "14084", "14106", "14111",
           "14113", "14139", "14141")
  expect_identical(getItems(toolOrderCells(a, dim = 1.4, na.rm = TRUE), dim = 1.4), out)
})
