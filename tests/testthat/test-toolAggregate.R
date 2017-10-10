context("Data aggregation")

data("population_magpie")
pm <- population_magpie
w <- pm
w[,,] <- NA
map <- data.frame(from=getRegions(pm),to=rep(c("REG1","REG2"),5))
map2 <- data.frame(from=getRegions(pm),to=getRegions(pm))

cfg <- getConfig(verbose = FALSE)

test_that("Identity mapping is not changing the data", {
  expect_equivalent(toolAggregate(pm,map2),pm)
})

test_that("NA columns in weight are summed up", {
  expect_equivalent(toolAggregate(pm,map),toolAggregate(pm,map, weight=w, mixed_aggregation=TRUE))
})

test_that("NA in weight leads to summation and other weight to weighting", {
  w[,,1] <- 1
  w[,,2] <- NA
  mix <- toolAggregate(pm,map, weight=w, mixed_aggregation=TRUE)
  mean <- toolAggregate(pm[,,1],map, weight=w[,,1])
  sum <- toolAggregate(pm[,,2],map)
  expect_equivalent(mix,mbind(mean,sum))
})

test_that("NAs in weight for mixed_aggregation=FALSE throw an error", {
  w[,,] <- NA
  expect_error(toolAggregate(pm,map, weight=w))
})

test_that("Random NAs in weight and mixed_aggregation=TRUE throw an error", {
  w[,,] <- 1
  w[3,1,1]  <- NA
  expect_error(toolAggregate(pm,map, weight=w, mixed_aggregation=TRUE))
})

test_that("partrel=TRUE works in combination with weights",{
  w[,,] <- NA
  map3 <- map[1:5,]
  expect_equivalent(toolAggregate(pm,map3,partrel=TRUE, verbosity=10),
                    toolAggregate(pm, map3, partrel = TRUE, weight=w[1:5,,], mixed_aggregation = TRUE, verbosity=10))
})

