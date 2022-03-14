p <- magclass::maxample("pop")
p[, , ] <- 1
getYears(p) <- 1900 + seq_len(nyears(p))

test_that("Proper detection of time step completness", {
  expect_equivalent(toolTimeAverage(p, averaging_range = 1, cut = FALSE), p)
})

test_that("Averaging works properly for trivial case", {
  expect_equivalent(toolTimeAverage(p, averaging_range = 4, cut = FALSE), p)
})

test_that("Averaging works independent of time step length", {
  p2 <- p3 <-  magclass::maxample("pop")
  getYears(p2) <- 1900 + seq_len(nyears(p2))
  getYears(p3) <- 1900 + seq_len(nyears(p3)) * 5
  expect_identical(toolTimeAverage(p2, averaging_range = 4, cut = FALSE),
                   setYears(toolTimeAverage(p3, averaging_range = 4, cut = FALSE), getYears(p2)))
})

test_that("Error detection works", {
  expect_error(toolTimeAverage(1), "not a MAgPIE object")
  expect_warning(toolTimeAverage(p, 0), "Invalid choice of averaging_range")
  expect_error(toolTimeAverage(p[, 1:2, ], 3), "greater than number of time steps")
  expect_error(toolTimeAverage(p[, c(1, 3, 4), ]), "equires equidistant years")
  expect_warning(toolTimeAverage(p[, c(1, 3), ], annual = FALSE), "deprecated")
})
