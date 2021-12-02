globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
}

test_that("downloadSource uses temporary downloadInProgress directory", {
  mainfolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  setConfig(mainfolder = mainfolder, globalenv = TRUE, .local = TRUE)

  downloadTest <- function() {
    expect_false(dir.exists(file.path(mainfolder, "sources", "Test")))
    expect_true(dir.exists(file.path(mainfolder, "sources", "Test-downloadInProgress")))
    return(list(url = "dummy", author = "dummy", title = "dummy", license = "dummy",
                description = "dummy", unit = "dummy"))
  }
  globalassign("downloadTest")

  expect_false(dir.exists(file.path(mainfolder, "sources", "Test")))
  downloadSource("Test")
  expect_true(dir.exists(file.path(mainfolder, "sources", "Test")))
})
