test_that("toolFixWeight works with one dimensional weight", {
  map <- data.frame(from = c("A", "A", "B", "B"),
                    to = c("A1", "A2", "B1", "B2"))
  weight <- new.magpie(map$to, fill = 0)
  weight["B1", , ] <- 1

  fixedWeight <- toolFixWeight(weight, map, dim = 1)
  expected <- new.magpie(c("A1", "A2", "B1", "B2"))
  expected[c("A1", "A2"), , ] <- 10^-30
  expected["B1", , ] <- 1
  expected["B2", , ] <- 0
  expect_identical(fixedWeight, expected)
})

test_that("toolFixWeight works with three dimensional weight", {
  map <- data.frame(from = c("CC", "CC", "CC", "DD", "DD"),
                    to = c("CC1", "CC2", "CC3", "DD1", "DD2"))
  weight <- new.magpie(c("A", "B"), map$to, c("EEE", "FFF"), fill = 0)
  weight["B", "CC3", "EEE"] <- 1

  fixedWeight <- toolFixWeight(weight, map, dim = 2)
  expected <- weight
  expected[] <- 10^-30
  expected["B", "CC3", "EEE"] <- 1
  expected["B", c("CC1", "CC2"), "EEE"] <- 0
  expect_identical(fixedWeight, expected)
})

test_that("toolFixWeight works with subdims", {
  map <- data.frame(from = c("CC", "CC", "CC", "DD", "DD"),
                    to = c("CC1", "CC2", "CC3", "DD1", "DD2"))
  weight <- new.magpie(c("A", "B"), paste0("X.", map$to), c("EEE", "FFF"), fill = 0)
  weight["B", "CC3", "EEE"] <- 1

  fixedWeight <- toolFixWeight(weight, map, dim = 2.2)
  expected <- weight
  expected[] <- 10^-30
  expected["B", "CC3", "EEE"] <- 1
  expected["B", c("CC1", "CC2"), "EEE"] <- 0
  expect_identical(fixedWeight, expected)
})

test_that("toolFixWeight can handle large objects", {
  skip("to save time skip test whether toolFixWeight can handle large objects")
  to <- Reduce(x = 1:26, init = NULL, f = function(total, i) {
    return(c(total, paste0(LETTERS[i], seq_len(1000 * i))))
  })

  map <- data.frame(from = substr(to, 1, 1), to = to)

  weight <- new.magpie(to, fill = 0)
  idx <- sample(seq_len(ncells(weight)), 20)
  weight[idx, , ] <- runif(length(idx))

  timed <- system.time(toolFixWeight(weight, map, dim = 1))
  expect_true(timed["elapsed"] < 5)
})
