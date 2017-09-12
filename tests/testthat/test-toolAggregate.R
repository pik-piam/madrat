context("Data aggregation")

data("population_magpie")
pm <- population_magpie
w <- pm
w[,,] <- NA
map <- data.frame(from=getRegions(pm),to=rep(c("REG1","REG2"),5))
map2 <- data.frame(from=getRegions(pm),to=getRegions(pm))

test_that("Identity mapping is not changing the data", {
  expect_equivalent(toolAggregate(pm,map2),pm)
})

test_that("NA columns in weight are summed up", {
  expect_equivalent(toolAggregate(pm,map),toolAggregate(pm,map, weight=w))
})

test_that("Mix of NA and 0 columns in weight are summed up", {
  w[,,1] <- 0
  expect_equivalent(toolAggregate(pm,map),toolAggregate(pm,map, weight=w))
})

test_that("0 columns in weight are summed up", {
  w[,,] <- 0
  expect_equivalent(toolAggregate(pm,map),toolAggregate(pm,map, weight=w))
})