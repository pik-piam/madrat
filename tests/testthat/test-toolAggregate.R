context("Data aggregation")

data("population_magpie")
pm <- population_magpie
w <- pm
w[,,] <- NA
map <- data.frame(from=getRegions(pm),reg=rep(c("REG1","REG2"),5),glo="GLO")
map2 <- data.frame(from=getRegions(pm),to=getRegions(pm))
#Spatial subdimension (trade data) objects
td <- new.magpie(paste(rep(getRegions(pm),nregions(pm)),rep(getRegions(pm),each=nregions(pm)),sep="."),getYears(pm),getNames(pm),pm)
tdeach <- new.magpie(paste(rep(getRegions(pm),each=nregions(pm)),rep(getRegions(pm),nregions(pm)),sep="."),getYears(pm),getNames(pm),pm)
rel <- data.frame(from=getRegions(pm),to=rep(c("REG1","REG2"),each=5))

cfg <- getConfig(verbose = FALSE)

test_that("Identity mapping is not changing the data", {
  expect_equivalent(toolAggregate(pm,map2),pm)
})

test_that("Combination via '+' works", {
  reg <- toolAggregate(pm,map,to="reg")
  glo <- toolAggregate(pm,map,to="glo")
  expect_equivalent(toolAggregate(pm,map,to="reg+glo"),mbind(reg,glo))
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
  expect_equivalent(magpiesort(toolAggregate(agg_tdeach,rel,weight=tdeach,dim=1.1,wdim=1.1)),magpiesort(tdeach))
})

test_that("disaggregation in dim=1.2 works appropriately",{
  agg_td <- toolAggregate(td,map,dim=1.2)
  map$glo <- NULL
  expect_equivalent(magpiesort(toolAggregate(agg_td,map,weight=td,dim=1.2,wdim=1.2)),magpiesort(td))
})

test_that("aggregating across dim=1.1 and then dim=1.2 produces the same result as vice versa",{
  agg_td1 <- toolAggregate(td,map,dim=1.1)
  agg_td2 <- toolAggregate(td,map,dim=1.2)
  expect_equivalent(magpiesort(toolAggregate(agg_td1,map,dim=1.2)),magpiesort(toolAggregate(agg_td2,map,dim=1.1)))
})

test_that("weight with reduced dimensionality can be used", {
  #skip("not yet fixed")
  unweighted <- toolAggregate(td,rel=map,dim=1.2)
  getSets(pm)[1] <- "region1"
  weighted <- toolAggregate(td,rel=map,weight=pm,dim=1.2)
  unweighted[,,] <- 1
  weighted[,,] <- 1
  expect_equivalent(unweighted,weighted)
})

test_that("toolAggregate does not get confused by identical sets", {
  #skip("not yet fixed")
  
  x <- new.magpie(paste(rep(c("A","B"),2),rep(c("A","B"),each=2),sep="."),1900,"blub",1:4)
  w <- new.magpie(c("A","B"),1900,"blub",c(0.1,0.9))
  rel <- data.frame(from=c("A","B"),to="GLO")
  
  out1 <- new.magpie(paste(rep("GLO",2),c("A","B"),sep="."),1900,"blub",c(3,7))
  expect_equivalent(toolAggregate(x,rel,dim=1.1),out1)
  
  out2 <- new.magpie(paste(rep("GLO",2),c("A","B"),sep="."),1900,"blub",c(4,6))
  expect_equivalent(toolAggregate(x,rel,dim=1.2),out2)
  
  wout1 <- new.magpie(paste(rep("GLO",2),c("A","B"),sep="."),1900,"blub",c(1.9,3.9))
  expect_equivalent(toolAggregate(x,rel,dim=1.1,weight=w),wout1) 
  
  wout2 <- new.magpie(paste(rep("GLO",2),c("A","B"),sep="."),1900,"blub",c(2.8,3.8))
  expect_equivalent(toolAggregate(x,rel,dim=1.2,weight=w),wout2) 
})

