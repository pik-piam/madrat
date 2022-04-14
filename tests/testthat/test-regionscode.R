test_that("regionscode function works", {
  localConfig(hash = "xxhash32", .verbose = FALSE)
  expect_identical(regionscode(), "62eff8f7")
  expect_identical(regionscode(strict = FALSE), "1bb3bfca")
  expect_identical(regionscode(label = TRUE), "h12")
  ref <- c(regionmappingH12.csv = "62eff8f7", regionmappingH12.csv = "62eff8f7")
  expect_identical(regionscode(c("regionmappingH12.csv", "regionmappingH12.csv")), ref)
  m <- toolGetMapping("regionmappingH12.csv")
  expect_identical(regionscode(m[, 2:3]), regionscode(m))
  expect_identical(regionscode(m[, 3:2]), regionscode(m))
})

test_that("Error detection works", {
  expect_error(regionscode(data.frame(from = 1:10, to = 1:10)),
               "less rows than there are ISO countries")
  expect_error(regionscode(data.frame(from = 1:1000, to = 1:1000)),
               "more rows than there are ISO countries")
  expect_error(regionscode(data.frame(from = 1:249, to = 1:249)),
               "does not contain a iso country column")
})
