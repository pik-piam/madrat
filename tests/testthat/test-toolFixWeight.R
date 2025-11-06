test_that("toolFixWeight works", {
  x <- new.magpie(c("A", "B"), fill = 100)
  rel <- data.frame(from = c("A", "A", "B", "B"),
                    to = c("A1", "A2", "B1", "B2"))
  weight <- new.magpie(c("A1", "A2", "B1", "B2"), fill = 0)
  weight["B1", , ] <- 1

  fixedWeight <- toolFixWeight(weight, rel, from = "from", to = "to", dim = 1)
  weightSum <- toolAggregate(fixedWeight, rel, from = "from", to = "to", dim = 1)
  expect_true(!any(weightSum == 0, na.rm = TRUE))
})
