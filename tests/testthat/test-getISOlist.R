test_that("getISOlist works", {
  expect_equal(length(getISOlist()), 249)
  expect_mapequal(c(getISOlist("important"), getISOlist("dispensable")), getISOlist())
})

test_that("data sets used in getISOlist are consistent to each other", {
  expect_silent({
    ref <- getISOlist()
  })
  pop2015 <- read.magpie(system.file("extdata", "pop2015.csv", package = "madrat"))
  expect_equal(length(ref), ncells(pop2015))
  expect_equal(unname(ref), getCells(pop2015))
  expect_error(getISOlist("blub"), "Unknown type")
})
