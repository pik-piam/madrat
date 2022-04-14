test_that("getSources works", {
  localConfig(globalenv = FALSE, .verbose = FALSE)
  expect_identical(getSources(name = "calcTauTotal", type = "download", packages = "madrat"), "Tau")
  expect_identical(getSources(type = "correct", packages = "madrat"), character(0))
  expect_identical(getSources(type = "read", packages = "madrat"), "Tau")
  expect_error(getSources(type = "blub", packages = "madrat"), "Unknown type")

  ref <- structure(list(source = "Tau", read = TRUE, correct = FALSE,
                        convert = TRUE, download = TRUE),
                   row.names = c(NA, -1L), class = "data.frame")
  expect_identical(getSources(packages = "madrat"), ref)
})
