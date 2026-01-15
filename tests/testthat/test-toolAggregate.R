pm <- magclass::maxample("pop")
w <- pm
w[, , ] <- NA
map <- data.frame(from = getItems(pm, dim = 1.1), reg = rep(c("REG1", "REG2"), 5), glo = "GLO")
map2 <- data.frame(from = getItems(pm, dim = 1.1), to = getItems(pm, dim = 1.1))
# Spatial subdimension (trade data) objects
td <- new.magpie(paste(rep(getItems(pm, dim = 1.1), nregions(pm)), rep(getItems(pm, dim = 1.1), each = nregions(pm)),
                       sep = "."),
                 getYears(pm), getNames(pm), pm)
tdeach <- new.magpie(paste(rep(getItems(pm, dim = 1.1), each = nregions(pm)),
                           rep(getItems(pm, dim = 1.1), nregions(pm)), sep = "."),
                     getYears(pm), getNames(pm), pm)
rel <- data.frame(from = getItems(pm, dim = 1.1), to = rep(c("REG1", "REG2"), each = 5))

cfg <- getConfig(verbose = FALSE)

noC <- function(x) {
  getComment(x) <- NULL
  attr(x, "Metadata") <- NULL # nolint: object_name_linter.
  return(x)
}

test_that("Identity mapping is not changing the data", {
  expect_equivalent(toolAggregate(pm, map2), pm)
  pimpf <- pm
  pimpf[2, 2005, ] <- Inf
  expect_equivalent(toolAggregate(pimpf, map2), pimpf)
  expect_identical(noC(toolAggregate(pm, diag(1, 10, 10))), noC(pm))
})

test_that("NAs and Infs in input data are treated correctly", {
  pm2 <- pm[c(1, 3, 5), 1:2, 1:2]
  pm2[1, 1, 1] <- Inf
  pm2[2, 2, 2] <- NA

  ref <- new("magpie",
             .Data = structure(c(Inf, 1837, 1559, NA), .Dim = c(1L, 2L, 2L),
                               .Dimnames = list(i = "REG1", t = c("y1995", "y2005"), scenario = c("A2", "B1"))))
  expect_identical(noC(round(toolAggregate(pm2, map, partrel = TRUE))), ref)
})

test_that("Mappings work in various formats identical", {
  expect_silent(ref <- noC(toolAggregate(pm, map)))
  expect_identical(noC(toolAggregate(pm, as.matrix(map))), ref)

  expect_identical(noC(toolAggregate(pm, map[, 3:1])), ref)
  expect_identical(noC(toolAggregate(pm, map[, 2:1])), ref)

  tmpfile <- file.path(withr::local_tempdir(), "map.rds")
  saveRDS(map, tmpfile)
  tmpfile2 <- file.path(withr::local_tempdir(), "map.csv")
  write.csv(map, tmpfile2)

  expect_identical(noC(toolAggregate(pm, tmpfile)), ref)
  expect_identical(noC(toolAggregate(pm, tmpfile2)), ref)

  skip_if_not_installed("tibble")
  expect_identical(noC(toolAggregate(pm, tibble::as_tibble(map))), ref)
})

test_that("Combination via '+' works", {
  reg <- noC(toolAggregate(pm, map, to = "reg"))
  glo <- noC(toolAggregate(pm, map, to = "glo"))
  expect_equivalent(noC(toolAggregate(pm, map, to = "reg+glo")), mbind(reg, glo))
})

test_that("NA columns in weight are summed up", {
  expect_equivalent(noC(toolAggregate(pm, map)), noC(toolAggregate(pm, map, weight = w, mixed_aggregation = TRUE)))
})

test_that("NA in weight leads to summation and other weight to weighting", {
  w[, , 1] <- 1
  w[, , 2] <- NA
  mix <- noC(toolAggregate(pm, map, weight = w, mixed_aggregation = TRUE))
  mean <- noC(toolAggregate(pm[, , 1], map, weight = w[, , 1]))
  sum <- noC(toolAggregate(pm[, , 2], map))
  expect_equivalent(mix, mbind(mean, sum))
})

test_that("NAs in weight for mixed_aggregation=FALSE throw an error", {
  w[, , ] <- NA
  expect_error(toolAggregate(pm, map, weight = w))
})

test_that("Random NAs in weight and mixed_aggregation=TRUE throw an error", {
  w[, , ] <- 1
  w[3, 1, 1]  <- NA
  expect_error(toolAggregate(pm, map, weight = w, mixed_aggregation = TRUE))
})

test_that("partrel=TRUE works in combination with weights", {
  w[, , ] <- NA
  map3 <- map[1:5, ]
  expect_equivalent(toolAggregate(pm, map3, partrel = TRUE, verbosity = 10),
                    toolAggregate(pm, map3, partrel = TRUE, weight = w[1:5, , ],
                                  mixed_aggregation = TRUE, verbosity = 10))
})

test_that("aggregation in dim=1.2 with regions-only mapping is the same as in dim=1 with region.cell mapping", {
  reltest <- data.frame(from = getCells(td), to = paste(rep(getItems(td, dim = 1.1), 10),
                                                        rep(c("REG1", "REG2"), each = 50), sep = "."))
  expect_equivalent(magpiesort(toolAggregate(td, rel, dim = 1.2)), magpiesort(toolAggregate(td, reltest, dim = 1)))
})

test_that("aggregation in dim=1.1 with regions-only mapping is the same as in dim=1 with region.cell mapping", {
  reltest <- data.frame(from = getCells(tdeach), to = paste(rep(c("REG1", "REG2"), each = 50),
                                                            rep(getItems(td, dim = 1.1), 10), sep = "."))
  expect_equivalent(magpiesort(toolAggregate(tdeach, rel, dim = 1.1)),
                    magpiesort(toolAggregate(tdeach, reltest, dim = 1)))
})

test_that("disaggregation in dim=1.1 works appropriately", {
  aggTdeach <- toolAggregate(tdeach, rel, dim = 1.1)
  expect_equivalent(magpiesort(toolAggregate(aggTdeach, rel, weight = tdeach, dim = 1.1, wdim = 1.1)),
                    magpiesort(tdeach))
})

test_that("disaggregation in dim=1.2 works appropriately", {
  aggTd <- toolAggregate(td, map, dim = 1.2)
  map$glo <- NULL
  expect_equivalent(magpiesort(toolAggregate(aggTd, map, weight = td, dim = 1.2, wdim = 1.2)), magpiesort(td))
})

test_that("aggregating across dim=1.1 and then dim=1.2 produces the same result as vice versa", {
  aggTd1 <- toolAggregate(td, map, dim = 1.1)
  aggTd2 <- toolAggregate(td, map, dim = 1.2)
  expect_equivalent(magpiesort(toolAggregate(aggTd1, map, dim = 1.2)),
                    magpiesort(toolAggregate(aggTd2, map, dim = 1.1)))
})

test_that("weight with reduced dimensionality can be used", {
  unweighted <- toolAggregate(td, rel = map, dim = 1.2)
  getSets(pm)[1] <- "region1"
  weighted <- toolAggregate(td, rel = map, weight = pm, dim = 1.2)
  unweighted[, , ] <- 1
  weighted[, , ] <- 1
  expect_equivalent(unweighted, weighted)
})

test_that("toolAggregate does not get confused by identical sets", {
  x <- new.magpie(paste(rep(c("A", "B"), 2), rep(c("A", "B"), each = 2), sep = "."), 1900, "blub", 1:4)
  w <- new.magpie(c("A", "B"), 1900, "blub", c(0.1, 0.9))
  rel <- data.frame(from = c("A", "B"), to = "GLO")

  out1 <- new.magpie(paste(rep("GLO", 2), c("A", "B"), sep = "."), 1900, "blub", c(3, 7))
  expect_equivalent(toolAggregate(x, rel, dim = 1.1), out1)

  out2 <- new.magpie(paste(rep("GLO", 2), c("A", "B"), sep = "."), 1900, "blub", c(4, 6))
  expect_equivalent(toolAggregate(x, rel, dim = 1.2), out2)

  wout1 <- new.magpie(paste(rep("GLO", 2), c("A", "B"), sep = "."), 1900, "blub", c(1.9, 3.9))
  expect_equivalent(toolAggregate(x, rel, dim = 1.1, weight = w), wout1)

  wout2 <- new.magpie(paste(rep("GLO", 2), c("A", "B"), sep = "."), 1900, "blub", c(2.8, 3.8))
  expect_equivalent(toolAggregate(x, rel, dim = 1.2, weight = w), wout2)
})

test_that("toolAggregate detects inconsistencies in inputs", {
  expect_error(suppressWarnings(toolAggregate(pm[1:3, , ], map2)), "Complete mapping failed.")
  expect_error(toolAggregate(pm[1:3, , ], map2, from = "from", to = "to"), "different number of entries")
})

test_that("aggregation for subdimensions works properly", {
  a <- magclass::maxample("animal")[1:3, 1:2, "black"]
  rel <- data.frame(from = c("rabbit", "bird"), to = "sweet")
  expect_identical(getItems(toolAggregate(a, rel, dim = "species"), dim = 3, full = TRUE), "animal.sweet.black")
})

test_that("aggregation with missing rel argument works", {
  a <- magclass::maxample("animal")
  rel <- data.frame(from = getItems(a, dim = 1), country = getItems(a, dim = "country", full = TRUE), global = "GLO")
  expect_identical(noC(toolAggregate(a, to = "country")), noC(toolAggregate(a, rel)))
  expect_identical(noC(toolAggregate(a, to = "global+country")), noC(toolAggregate(a, to = "global+country")))
  expect_identical(noC(toolAggregate(a, to = "species", dim = 3)), noC(dimSums(a, dim = c(3.1, 3.3))))
  aSum <- dimSums(a, dim = 3)
  getSets(aSum, fulldim = FALSE)[3] <- "all"
  getItems(aSum, dim = 3) <- "all"
  expect_identical(noC(toolAggregate(a, to = "all", dim = 3)), noC(aSum))
  expect_equivalent(toolAggregate(a, to = "global"), toolAggregate(a, to = "all"))
})

test_that("Malformed inputs are properly detected", {
  expect_error(toolAggregate(1, 2), "Input is not a MAgPIE object")
  expect_error(toolAggregate(pm, map, weight = 1), "Weight is not a MAgPIE object")
  expect_error(toolAggregate(as.magpie(1), rel = "notthere.csv"),
               paste0("Cannot find region mapping file: notthere.csv (working directory ", getwd(), ")"), fixed = TRUE)
  expect_error(toolAggregate(pm, map[, 1, drop = FALSE]), "has only 1 column")
  expect_error(toolAggregate(pm, map[, 1]), "Malformed relation mapping")
  expect_error(toolAggregate(pm, map, weight = pm[1:2, , ]), "no match")
  w <- pm
  getCells(w) <- paste0(getCells(pm), ".", getCells(pm)[10:1])
  expect_error(toolAggregate(pm, map, weight = w), "multiple matches")
  expect_warning(toolAggregate(pm, map, weight = -pm), "Negative numbers in weight.")
  expect_error(toolAggregate(pm, map, weight = -pm, negative_weight = "stop"), "Negative numbers in weight.")
  expect_error(toolAggregate(pm, diag(1, 16, 16), dim = 2), "Missing dimnames for aggregated dimension")
  expect_error(toolAggregate(magclass::maxample("animal"), to = "country", dim = 1.2),
               "Subdimensions in dim not supported if relation mapping is missing!")
})

test_that("Edge cases work", {
  rel <- diag(1, 10, 4)
  colnames(rel) <- c("A", "A", "B", "B")
  expect_identical(getItems(toolAggregate(pm, rel), 1), c("A.1", "A.2", "B.3", "B.4"))

  a <- magclass::maxample("animal")
  rel2 <- diag(1, 3, 3)
  expect_error(toolAggregate(a, rel2, dim = 3.2), "colnames and/or rownames missing")
  colnames(rel2) <- paste0("A", 1:3)
  rownames(rel2) <- paste0("B", 1:3)
  expect_error(toolAggregate(a, rel2, dim = 3.2), "not be found in the data")
  rownames(rel2) <- c("dog", "bird", "rabbit")
  expect_identical(getItems(toolAggregate(a, rel2, dim = 3.2), 3.2), paste0("A", 1:3))
  colnames(rel2) <- c("dog", "bird", "rabbit")
  expect_identical(getItems(toolAggregate(a, rel2[1:2, ], dim = 3.2), 3.2), c("dog", "bird"))

  a <- collapseDim(a, dim = c(1.1, 1.2))
  rel <- data.frame(from = getCells(a), to = getItems(a, dim = 1.1, full = TRUE), stringsAsFactors = FALSE)
  expect_silent(b <- toolAggregate(a, rel, weight = a))
  expect_setequal(getCells(b), c("NLD", "BEL", "LUX"))
})

test_that("columns with only zeros in weight produce a warning", {
  weight <- pm
  weight[, , ] <- 0
  expect_warning(toolAggregate(pm, rel, weight = weight), "Weight sum is 0")
  expect_error(toolAggregate(pm, rel, weight = weight, zeroWeight = "stop"), "Weight sum is 0")
  expect_silent(allZero <- toolAggregate(pm, rel, weight = weight, zeroWeight = "allow"))
  expect_true(all(allZero == 0))
  expect_silent(setNA <- toolAggregate(pm, rel, weight = weight, zeroWeight = "setNA"))
  expect_true(all(is.na(setNA)))
})

test_that("trivial renaming works with weight", {
  mapping <- data.frame(a = c("A2", "B1"), b = c("C", "D"))
  weight <- pm
  getItems(weight, 3) <- c("C", "D")
  expect_silent(toolAggregate(pm, mapping, from = "a", to = "b", dim = 3, weight = weight))
})

test_that("zeroWeight = fix works", {
  x <- new.magpie(c("A", "B"), fill = 100)
  rel <- data.frame(c("A", "A", "B", "B"),
                    c("A1", "A2", "B1", "B2"))
  weight <- new.magpie(rel[[2]], fill = 0)
  weight["B1", , ] <- 1

  expect_warning(y <- toolAggregate(x, rel, weight),
                 "Weight sum is 0")
  expect_true(sum(x) - sum(y) == 100) # total sum no longer equal, hence the warning
  expect_true(as.vector(y["B2", , ]) == 0)

  # problematic hotfix
  expect_silent(y <- toolAggregate(x, rel, weight + 10^-10))
  expect_equal(sum(x), sum(y)) # total sum is equal
  expect_true(as.vector(y["B2", , ]) > 0) # this is the problem, should still be 0

  # proper fix
  expect_silent(y <- toolAggregate(x, rel, weight, zeroWeight = "fix"))
  expect_equal(sum(x), sum(y)) # total sum is equal
  expect_true(as.vector(y["B2", , ]) == 0) # and this is also still 0
})

test_that("empty cells in a to column do not result in aggregated data", {

  empty <- ""

  ## dim 1
  localMap <- map
  localMap$TestReg <- c(rep(empty, 7), rep("PRegion", 2), empty)
  expected <- pm["PAO", , ] + pm["PAS", , ]
  getSets(expected, fulldim = FALSE)[1] <- "i"
  getItems(expected, 1) <- "PRegion"
  expect_equal(noC(toolAggregate(pm, localMap, to = "TestReg")),
               noC(expected))

  ### toolAggregate throws a note
  expect_message(
    toolAggregate(pm, localMap, to = "TestReg"),
    ".*Aggregation target included \"\"\\. Those items were removed from aggregation result\\."
  )

  ### via mapping file
  tmpfile <- file.path(withr::local_tempdir(), "map.rds")
  saveRDS(localMap, tmpfile)
  expect_equal(noC(toolAggregate(pm, tmpfile, to = "TestReg")),
               noC(expected))

  ## dim 1 and weighted
  x <- new.magpie(c("A", "B", "C"), fill = 100)
  x["A"] <- 50
  rel <- data.frame(c("A", "B", "C"),
                    c("AGG", "AGG", ""))
  weight <- new.magpie(getItems(x, 1), fill = c(3, 1, 4))

  expect_equal(noC(toolAggregate(x, rel, weight)),
               new.magpie("AGG", fill = 62.5))

  ## dim 2
  localMap <- data.frame(scenario = c("A2", "B1"))
  localMap$TestCategory <- c(empty, "NewCategory")

  expected <- pm[, , "B1"]
  getItems(expected, 3) <- "NewCategory"
  expect_equal(noC(toolAggregate(pm, localMap, dim = 3, to = "TestCategory")),
               noC(expected))

  ### via mapping file
  tmpfile <- file.path(withr::local_tempdir(), "map.rds")
  saveRDS(localMap, tmpfile)
  expect_equal(noC(toolAggregate(pm, tmpfile, dim = 3, to = "TestCategory")),
               noC(expected))

  ## subdim
  x <- new.magpie(c("A.SUB1", "A.SUB2", "B.SUB1", "B.SUB2", "C.SUB1"), fill = 100)
  rel <- data.frame(from = c("A.SUB1", "A.SUB2", "B.SUB1", "B.SUB2", "C.SUB1"),
                    region = c("", "AGG", "", "AGG", ""))

  expect_equal(noC(toolAggregate(x, rel)),
               new.magpie("AGG", fill = 200))

  ## Existing empty string items in non-aggregated dimensions
  ## are not affected.
  x <- new.magpie(c("A", "B", "C"), "", fill = 100)
  rel <- data.frame(c("A", "B", "C"),
                    c("AGG", "AGG", ""))

  expect_equal(noC(toolAggregate(x, rel)),
               new.magpie("AGG", "", fill = 200))

  ## Other representations of empty values
  ## should not be removed
  x <- new.magpie(c("A", "B", "C"), "", fill = 100)
  rel <- data.frame(c("A", "B", "C"),
                    c("AGG", "", "NULL"))

  expect_equal(noC(toolAggregate(x, rel)),
               new.magpie(c("AGG", "NULL"), "", fill = 100))

})
