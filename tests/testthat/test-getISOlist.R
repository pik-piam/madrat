# SPDX-FileCopyrightText: 2025 Potsdam Institute for Climate Impact Research (PIK)
# SPDX-License-Identifier: BSD-2-Clause

test_that("getISOlist works", {
  expect_equal(length(getISOlist()), 249)
  expect_mapequal(c(getISOlist("important"), getISOlist("dispensable")), getISOlist())
})

test_that("data sets used in getISOlist are consistent to each other", {
  expect_silent({
    ref <- getISOlist()
  })
  pop2015 <- readRDS(system.file("extdata", "pop2015.rds", package = "madrat"))
  expect_equal(length(ref), ncells(pop2015))
  expect_equal(unname(ref), getCells(pop2015))
  expect_error(getISOlist("blub"), "Unknown type")
})
