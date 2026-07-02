test_that("metadata can be extracted from GFZ dataservice", {
  # mock the network request so no real download from doi.org/GFZ is needed
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines(c(
        paste0("<span class=\"citationtext\">Lange, Stefan (2019): EartH2Observe, WFDEI and ERA-Interim data",
               " Merged and Bias-corrected for ISIMIP (EWEMBI). V. 1.1. GFZ Data Services.",
               " https://doi.org/10.5880/pik.2019.004</span>"),
        "<dt>License:</dt>",
        "<dd>CC BY 4.0</dd>"
      ), destfile)
      0L
    },
    .package = "madrat"
  )

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

test_that("metadataGFZ warns when citation or license cannot be extracted", {
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("<html><body>no metadata here</body></html>", destfile)
      0L
    },
    .package = "madrat"
  )

  w <- capture_warnings(m <- metadataGFZ("10.5880/pik.2019.004"))
  expect_match(w, "Cannot extract citation", all = FALSE)
  expect_match(w, "Cannot extract license", all = FALSE)
  expect_null(m$citation)
  expect_null(m$license)
})
