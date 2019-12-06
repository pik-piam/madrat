context("Data aggregation")

data("population_magpie")
pm <- population_magpie
w <- pm
w[,,] <- NA
map <- data.frame(from=getRegions(pm),to=rep(c("REG1","REG2"),5))
map2 <- data.frame(from=getRegions(pm),to=getRegions(pm))
#Spatial subdimension (trade data) objects
td <- new.magpie(paste(rep(getRegions(pm),nregions(pm)),rep(getRegions(pm),each=nregions(pm)),sep="."),getYears(pm),getNames(pm),pm)
tdeach <- new.magpie(paste(rep(getRegions(pm),each=nregions(pm)),rep(getRegions(pm),nregions(pm)),sep="."),getYears(pm),getNames(pm),pm)
rel <- data.frame(from=getRegions(pm),to=rep(c("REG1","REG2"),each=5))

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

test_that("aggregation in dim=1.2 with regions-only mapping is the same as in dim=1 with region.cell mapping",{
  reltest <- data.frame(from=getCells(td),to=paste(rep(getRegions(td),10),rep(c("REG1","REG2"),each=50),sep="."))
  expect_equivalent(magpiesort(toolAggregate(td,rel,dim=1.2)),magpiesort(toolAggregate(td,reltest,dim=1)))
})

test_that("aggregation in dim=1.1 with regions-only mapping is the same as in dim=1 with region.cell mapping",{
  reltest <- data.frame(from=getCells(tdeach),to=paste(rep(c("REG1","REG2"),each=50),rep(getRegions(td),10),sep="."))
  expect_equivalent(magpiesort(toolAggregate(tdeach,rel,dim=1.1)),magpiesort(toolAggregate(tdeach,reltest,dim=1)))
})

test_that("disaggregation in dim=1.1 works appropriately",{
  agg_tdeach <- toolAggregate(tdeach,rel,dim=1.1)
  expect_equivalent(magpiesort(toolAggregate(agg_tdeach,rel,weight=tdeach,dim=1.1)),magpiesort(tdeach))
})

test_that("disaggregation in dim=1.2 works appropriately",{
  agg_td <- toolAggregate(td,map,dim=1.2)
  expect_equivalent(magpiesort(toolAggregate(agg_td,map,weight=td,dim=1.2)),magpiesort(td))
})

test_that("aggregating across dim=1.1 and then dim=1.2 produces the same result as vice versa",{
  agg_td1 <- toolAggregate(td,map,dim=1.1)
  agg_td2 <- toolAggregate(td,map,dim=1.2)
  expect_equivalent(magpiesort(toolAggregate(agg_td1,map,dim=1.2)),magpiesort(toolAggregate(agg_td2,map,dim=1.1)))
})

test_that("weight with reduced dimensionality can be used", {
  skip("not yet fixed")
  unweighted <- toolAggregate(td,rel=map,dim=1.2)
  weighted <- toolAggregate(td,rel=map,weight=pm,dim=1.2)
  unweighted[,,] <- 1
  weighted[,,] <- 1
  expect_identical(unweighted,weighted)
})
