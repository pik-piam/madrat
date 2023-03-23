test_that("setTerraOptions works", {
  originalTerraOptions <- terra::terraOptions(print = FALSE)
  f <- function() {
    expect_identical(terra::terraOptions(print = FALSE), originalTerraOptions)

    setTerraOptions()

    expect_identical(terra::terraOptions(print = FALSE)$memfrac, 0.5)
    expect_identical(terra::terraOptions(print = FALSE)$todisk, TRUE)
    expect_true(terra::terraOptions(print = FALSE)$tempdir != tempdir())
  }

  f()
  expect_identical(terra::terraOptions(print = FALSE), originalTerraOptions)
})
