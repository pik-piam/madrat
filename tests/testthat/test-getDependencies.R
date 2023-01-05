test_that("getDependencies works for edge cases", {
  readTestX <- function() return(1)
  globalassign("readTestX")
  graph <- getMadratGraph(packages = "madrat")
  expect_null(getDependencies("readTestX", graph = graph))
  ref <- data.frame(func = "readTestX", type = "read", package = ".GlobalEnv",
                    call = "readTestX", hash = "783a5e2f",
                    row.names = NULL, stringsAsFactors = FALSE)
  expect_identical(getDependencies("readTestX", self = TRUE, graph = graph), ref)
  o <- list()
  for (d in c("full", "all", "both", "din", "dout", "in", "out")) {
    expect_silent(o[[d]] <- getDependencies("calcTauTotal", direction = d, graph = graph))
  }
  expect_identical(o$full, o$all)
  expect_identical(o$out, o$dout)
})
