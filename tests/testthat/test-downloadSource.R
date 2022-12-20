test_that("downloadSource uses temporary downloadInProgress directory", {
  mainfolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  localConfig(mainfolder = mainfolder)

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

test_that("downloadSource waits until already running download is finished", {
  mainfolder <- normalizePath(withr::local_tempdir(), winslash = "/")
  localConfig(mainfolder = mainfolder)
  dir.create(file.path(mainfolder, "sources", "Tau-downloadInProgress"), recursive = TRUE)

  expect_error(downloadSource("Tau", numberOfTries = 1),
               paste("The download did not finish in time. If the download is no longer running delete",
                     file.path(mainfolder, "sources", "Tau-downloadInProgress")), fixed = TRUE)
  dir.create(file.path(mainfolder, "sources", "Tau"), recursive = TRUE)
  expect_error(downloadSource("Tau", numberOfTries = 1.45),
               "as.integer(numberOfTries) == numberOfTries is not TRUE", fixed = TRUE)
  expect_error(downloadSource("Tau", numberOfTries = 1:3), "length(numberOfTries) == 1 is not TRUE", fixed = TRUE)
  expect_error(downloadSource("Tau", numberOfTries = -1), "numberOfTries >= 1 is not TRUE", fixed = TRUE)
  withr::with_options(list(warn = 2), {# turn warnings into error so execution is stopped after warning
    expect_error(downloadSource("Tau", numberOfTries = 1),
                 paste("The source folders Tau and Tau-downloadInProgress should not exist at the same time.",
                       "Please delete", file.path(mainfolder, "sources", "Tau-downloadInProgress")),
                 fixed = TRUE)
  })
})
