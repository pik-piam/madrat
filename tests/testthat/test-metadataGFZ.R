test_that("metadata can be extracted from GFZ dataservice", {
  skip_on_cran()
  skip_if_offline("doi.org")
  expect_silent({
    m <- metadataGFZ("10.5880/pik.2019.004")
  })
  mref <- list(citation = paste0("Lange, Stefan (2019): EartH2Observe, WFDEI and ERA-Interim data Merged",
                                 " and Bias-corrected for ISIMIP (EWEMBI). V. 1.1. GFZ Data Services.",
                                 " https://doi.org/10.5880/pik.2019.004"),
               authors = structure(list(list(given = "Stefan", family = "Lange",
                                             role = NULL, email = NULL, comment = NULL)), class = "person"),
               year = "EWEMBI", license = "CC BY 4.0")
  expect_identical(m, mref)
  expect_error(metadataGFZ("12.3456/pik.2019.004"), "does not belong to a GFZ dataservice entry")
  expect_null(metadataGFZ(NULL))
})
