context("getDependencies")

globalassign <- function(...) {
  for(x in c(...)) assign(x,eval.parent(parse(text=x)),.GlobalEnv)
}

test_that("getDependencies works for edge cases", {
  readTestX <- function()return(1)
  globalassign("readTestX")
  graph <- getMadratGraph(packages = "madrat", globalenv = TRUE)
  expect_null(getDependencies("readTestX", graph = graph))
  ref <- data.frame(func = "readTestX", type = "read", package = ".GlobalEnv",
                    row.names=NULL, stringsAsFactors = FALSE)
  expect_identical(getDependencies("readTestX", self=TRUE, graph = graph), ref)
})

