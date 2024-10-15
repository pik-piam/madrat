test_that("Historic data is properly translated", {
  localConfig(.verbose = FALSE)
  newcountries <- c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ",
                    "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB")
  a <- new.magpie(c("SUN", newcountries), 1990:1992)
  a["SUN", 1990, ] <- 240
  a[-1, 1991:1992, ] <- rep(1:15, 2)
  a <- toolISOhistorical(a)
  expect_equal(as.vector(a[, 1990, ]), 2 * (1:15))
  expect_identical(magclass::getItems(a, dim = 1.1), newcountries)
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
                                         .Dim = c(4L, 5L, 1L),
                                         .Dimnames = list(region = c("A", "B1", "B2", "B3"),
                                                          year = c("y9", "y10", "y11", "y12", "y13"),
                                                          data = NULL)))
  expect_identical(toolISOhistorical(a, mapping = m), ref)

  mapfile <- tempfile()
  write.table(m, mapfile, sep = ";")
  expect_identical(toolISOhistorical(a, mapping = mapfile), ref)
  expect_identical(toolISOhistorical(a, additional_mapping = m), ref)

  m2 <- as.list(as.data.frame(t(m), stringsAsFactors = FALSE))
  expect_identical(toolISOhistorical(a, additional_mapping = m2), ref)

  expect_error(toolISOhistorical(a["B1", , invert = TRUE], mapping = m), "there is no data for")

  b <- a
  b[is.na(b)] <- 0

  ref2 <- new("magpie", .Data = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 2,
                                            0, 0, 0, 2, 0, 0, 0, 2, 1, 1, 1),
                                          .Dim = c(4L, 5L, 1L),
                                          .Dimnames = list(region = c("A", "B1", "B2", "B3"),
                                                           year = c("y9", "y10", "y11", "y12", "y13"),
                                                           data = NULL)))

  expect_identical(toolISOhistorical(b, mapping = m, overwrite = TRUE), ref)
  expect_identical(toolISOhistorical(b, mapping = m, overwrite = FALSE), ref2)

  a["B1", 13, ] <- NA
  expect_warning(o <- toolISOhistorical(a, mapping = m), "Weight in toolISOhistorical contained NAs")
  ref3 <- new("magpie", .Data = structure(c(2, 0, 1.5, 1.5, 2, 0, 1.5,
                                            1.5, 2, 0, 1.5, 1.5, 2, 0, 1.5, 1.5, 2, NA, 1, 1),
                                          .Dim = c(4L, 5L, 1L),
                                          .Dimnames = list(region = c("A", "B1", "B2", "B3"),
                                                           year = c("y9", "y10", "y11", "y12", "y13"),
                                                           data = NULL)))
  expect_equal(o, ref3)
})

test_that("Bilateral data is properly translated", {
  localConfig(.verbose = FALSE)
  newcountriesA <- as.vector(outer(c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ",
                                     "LVA", "LTU", "MDA", "TJK", "TKM", "UKR", "UZB"), "USA", paste, sep = "."))
  missing <- "RUS.USA"
  a <- new.magpie(c("SUN.USA", newcountriesA), 1991:1993)
  a["SUN.USA", 1991, ] <- 240
  a[-1, 1992:1993, ] <- 1
  a <- toolISOhistorical(a, overwrite = TRUE)
  expect_equal(as.vector(a[, 1991, ]), c(rep(240 / 14, 14), 0))
  expect_identical(magclass::getItems(a, dim = 1), c(newcountriesA, missing))

  # when disagg in 2nd dim, and with all countries missing
  b <- new.magpie("USA.SUN", 1991:1993)
  b["USA.SUN", 1991, ] <- 240
  for (ov in c(TRUE, FALSE)) {
    b1 <- toolISOhistorical(b, overwrite = ov)
    expect_equal(as.vector(b1[, 1991, ]), rep(240 / 15, 15))
  }
  # when additional weighting is given
  expect_error(o <- toolISOhistorical(b, additional_weight = "2"),
               "Additional_weight is not supported for bilateral data.")

  # when aggregating
  mapping <- data.frame(fromISO = c("A1", "A2"),
                        toISO = c("A", "A"),
                        lastYear = c("y0010", "y0010"),
                        stringsAsFactors = FALSE)

  d <- new.magpie(c("A1.Z", "A2.Z", "A.Z"), 9:11, )
  d[c("A1.Z", "A2.Z"), c("y0009", "y0010"), ] <- 5
  d <- toolISOhistorical(d, mapping = mapping, overwrite = FALSE)
  expect_equal(as.vector(d[, c("y0009", "y0010"), ]), rep(10, 2))



})
