test_that("toolFixWeight works with one dimensional weight", {
  x <- new.magpie(c("A", "B"), fill = 100)
  rel <- data.frame(from = c("A", "A", "B", "B"),
                    to = c("A1", "A2", "B1", "B2"))
  weight <- new.magpie(rel$to, fill = 0)
  weight["B1", , ] <- 1

  fixedWeight <- toolFixWeight(weight, rel, dim = 1)
  expected <- new.magpie(c("A1", "A2", "B1", "B2"))
  expected[c("A1", "A2"), , ] <- 10^-30
  expected["B1", , ] <- 1
  expected["B2", , ] <- 0
  expect_identical(fixedWeight, expected)
})

test_that("toolFixWeight works with three dimensional weight", {
  x <- new.magpie(c("A", "B"), c("CC", "DD"), c("EEE", "FFF"), fill = 100)
  rel <- data.frame(from = c("CC", "CC", "CC", "DD", "DD"),
                    to = c("CC1", "CC2", "CC3", "DD1", "DD2"))
  weight <- new.magpie(getItems(x, 1), rel$to, getItems(x, 3), fill = 0)
  weight["B", "CC3", "EEE"] <- 1

  fixedWeight <- toolFixWeight(weight, rel, dim = 2)
  expected <- weight
  expected[] <- 10^-30
  expected["B", "CC3", "EEE"] <- 1
  expected["B", c("CC1", "CC2"), "EEE"] <- 0
  expect_identical(fixedWeight, expected)
})
