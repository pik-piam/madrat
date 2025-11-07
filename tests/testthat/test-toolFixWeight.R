test_that("toolFixWeight works", {
  x <- new.magpie(c("A", "B"), fill = 100)
  rel <- data.frame(from = c("A", "A", "B", "B"),
                    to = c("A1", "A2", "B1", "B2"))
  weight <- new.magpie(rel$to, fill = 0)
  weight["B1", , ] <- 1

  fixedWeight <- toolFixWeight(weight, rel, from = "from", to = "to", dim = 1)
  expected <- new.magpie(c("A1", "A2", "B1", "B2"))
  expected[c("A1", "A2"), , ] <- 10^-30
  expected["B1", , ] <- 1
  expected["B2", , ] <- 0
  expect_identical(fixedWeight, expected)
})
