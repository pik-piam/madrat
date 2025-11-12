test_that("toolFixWeight works with one dimensional weight", {
  x <- new.magpie(c("A", "B"), fill = 100)
  map <- c(A1 = "A",
           A2 = "A",
           B1 = "B",
           B2 = "B")
  weight <- new.magpie(names(map), fill = 0)
  weight["B1", , ] <- 1

  fixedWeight <- toolFixWeight(weight, map, dim = 1)
  expected <- new.magpie(c("A1", "A2", "B1", "B2"))
  expected[c("A1", "A2"), , ] <- 10^-30
  expected["B1", , ] <- 1
  expected["B2", , ] <- 0
  expect_identical(fixedWeight, expected)
})

test_that("toolFixWeight works with three dimensional weight", {
  x <- new.magpie(c("A", "B"), c("CC", "DD"), c("EEE", "FFF"), fill = 100)
  map <- c(CC1 = "CC",
           CC2 = "CC",
           CC3 = "CC",
           DD1 = "DD",
           DD2 = "DD")
  weight <- new.magpie(getItems(x, 1), names(map), getItems(x, 3), fill = 0)
  weight["B", "CC3", "EEE"] <- 1

  fixedWeight <- toolFixWeight(weight, map, dim = 2)
  expected <- weight
  expected[] <- 10^-30
  expected["B", "CC3", "EEE"] <- 1
  expected["B", c("CC1", "CC2"), "EEE"] <- 0
  expect_identical(fixedWeight, expected)
})
