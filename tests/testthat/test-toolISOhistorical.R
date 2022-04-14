test_that("Historic data is properly translated", {
  localConfig(.verbose = FALSE)
  newcountries <- c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ",
                    "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB")
  testData <- new.magpie(c("SUN", newcountries), 1990:1992)
  testData["SUN", 1990, ] <- 240
  testData[-1, 1991:1992, ] <- rep(1:15, 2)
  result <- toolISOhistorical(testData)
  expect_identical(as.vector(result[, 1990, ]), 2 * (1:15))
  expect_identical(magclass::getItems(result, dim = 1.1), newcountries)

  testData[-1, 1990, ] <- 0
  expect_warning(toolISOhistorical(testData),
                 paste("Not replacing data for",
                       "[ARM, AZE, BLR, EST, GEO, KAZ, KGZ, LVA, LTU, MDA, RUS, TJK, TKM, UKR, UZB]",
                       "for the years [y1990] although it is all zeros. Call toolISOhistorical with",
                       "overwrite = TRUE/FALSE to get the desired behavior without this warning."), fixed = TRUE)

  result <- toolISOhistorical(testData, overwrite = FALSE)
  expect_identical(as.vector(result[, 1990, ]), rep(0, 15))

  result <- toolISOhistorical(testData, overwrite = TRUE)
  expect_identical(as.vector(result[, 1990, ]), 2 * (1:15))
})

test_that("Given mapping data is properly translated", {
  m <- data.frame(fromISO = c("A1", "A2", "B", "B", "B"),
                  toISO = c("A", "A", "B1", "B2", "B3"),
                  lastYear = c("y10", "y10", "y12", "y12", "y12"),
                  stringsAsFactors = FALSE)
  a <- new.magpie(unique(c(m$fromISO, m$toISO)), paste0("y", 9:13))
  a[c("A1", "A2"), 9:10, ] <- 1
  a["A", 11:13, ] <- 2
  a["B", 9:12, ]  <- 3
  a[c("B1", "B2", "B3"), 13, ] <- 1

  ref <- new("magpie", .Data = structure(c(2, 1, 1, 1, 2, 1, 1, 1, 2,
                                           1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1),
                                         .Dim = c(4L, 5L, 1L), .Dimnames = list(region = c("A", "B1", "B2", "B3"),
                                                                                year = paste0("y", 9:13), data = NULL)))
  expect_identical(toolISOhistorical(a, mapping = m), ref)

  mapfile <- tempfile()
  write.table(m, mapfile, sep = ";")
  expect_identical(toolISOhistorical(a, mapping = mapfile), ref)
  expect_identical(toolISOhistorical(a, additional_mapping = m), ref)

  expect_error(toolISOhistorical(a, mapping = mapfile, additional_mapping = m),
               paste0("(toolISOhistorical) The following rows are duplicated ",
                      "after combining mapping and additional_mapping:\n",
                      'c(fromISO = "A1", toISO = "A", lastYear = "y10")\n',
                      'c(fromISO = "A2", toISO = "A", lastYear = "y10")\n',
                      'c(fromISO = "B", toISO = "B1", lastYear = "y12")\n',
                      'c(fromISO = "B", toISO = "B2", lastYear = "y12")\n',
                      'c(fromISO = "B", toISO = "B3", lastYear = "y12")'), fixed = TRUE)

  m2 <- as.list(as.data.frame(t(m), stringsAsFactors = FALSE))
  expect_identical(toolISOhistorical(a, additional_mapping = m2), ref)

  expect_error(toolISOhistorical(a["B1", , invert = TRUE], mapping = m),
               paste(c("The following transition failed:",
                       "list(fromISO = \"B\", toISO = c(\"B1\", \"B2\", \"B3\"), fromYear = \"y12\", toYear = \"y13\")",
                       "Missing disaggregation weights for [B1].",
                       "Here's the available data:",
                       "      year",
                       "region y10 y11 y12 y13",
                       "    B    3   3   3  NA",
                       "    B1  NA  NA  NA  NA",
                       "    B2  NA  NA  NA   1",
                       "    B3  NA  NA  NA   1",
                       "Provide explicit weights to toolISOhistorical by passing",
                       "additional_weight = as.magpie(c(B1 = ?))"), collapse = "\n"),
               fixed = TRUE)

  b <- a
  b[is.na(b)] <- 0

  ref2 <- new("magpie", .Data = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 2,
                                            0, 0, 0, 2, 0, 0, 0, 2, 1, 1, 1), .Dim = c(4L, 5L, 1L),
                                          .Dimnames = list(region = c("A", "B1", "B2", "B3"),
                                                           year = c("y9", "y10", "y11", "y12", "y13"), data = NULL)))

  expect_identical(toolISOhistorical(b, mapping = m, overwrite = TRUE), ref)
  expect_identical(toolISOhistorical(b, mapping = m, overwrite = FALSE), ref2)
})

test_that("toolISOhistorical recognizes fractional data", {
  testData <- new.magpie(c("SCG", "SRB", "MNE"), 2004:2006, c("a", "b"))
  testData["SCG", 2004, ] <- 0.4
  testData["SCG", 2005, ] <- 0.2
  testData[c("SRB", "MNE"), 2005, ] <- rep(0.1, 4)
  testData[c("SRB", "MNE"), 2006, ] <- rep(0.2, 4)
  expect_warning(toolISOhistorical(testData),
                 paste0("All data for x[, , c(1, 2)] is <= 1 or NA. ",
                        "If that data is fractional toolISOhistorical must not be used on it. ",
                        "If the data is not fractional, pass checkFractional = FALSE when calling toolISOhistorical."),
                 fixed = TRUE)
})
