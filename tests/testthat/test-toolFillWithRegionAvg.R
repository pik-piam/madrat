context("toolFillWithRegionAvg")

p <- magclass::maxample("pop")

test_that("Fill with regional average works as expected", {
  x <- magclass::new.magpie(cells_and_regions = c('A','B','C','D'), years = c(2000,2005), 
                            fill = c(1,NA,3,4,5,6,NA,8))
  rel <- data.frame(CountryCode = c('A','B','C','D'), RegionCode = c('R1','R1','R1','R2'))
  xfilled <- toolFillWithRegionAvg(x, regionmapping = rel, verbose=FALSE)
  expect_equal(as.vector(xfilled["B",2000,1]),2)
  expect_equal(as.vector(xfilled["C",2005,1]),11/2)
  expect_true(all(!is.na(xfilled)))
})

test_that("Malformed inputs are properly detected", {
  expect_error(toolFillWithRegionAvg(1), "has to be a MAgPIE object")
  expect_error(toolFillWithRegionAvg(p), "Only one element")
  expect_error(toolFillWithRegionAvg(p[,,1], weight = p), "must have exactly one")
  expect_error(toolFillWithRegionAvg(p[,,1], weight = p[2:5,,1]), "Regions .* do not match")
  expect_error(toolFillWithRegionAvg(p[,,1], weight = p[,2:3,1]), "Years .* do not match")
})
