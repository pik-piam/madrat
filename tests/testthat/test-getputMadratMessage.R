test_that("getMadratMessage and putMadratMessage work", {
  localConfig(globalenv = FALSE, .verbose = FALSE)
  expect_silent(resetMadratMessages())
  expect_silent(putMadratMessage("test", "This is a test", fname = "example"))
  expect_silent(putMadratMessage("test", "This is a toast", fname = "readTau"))
  expect_identical(getOption("madratMessage"), list(test = list(example = "This is a test",
                                                                readTau = "This is a toast")))
  expect_identical(getMadratMessage("test", fname = "calcTauTotal"), list(readTau = "This is a toast"))
  expect_silent(resetMadratMessages())
  expect_null(getMadratMessage())
  expect_silent(putMadratMessage("test2", "another test", fname = "convertTau"))
  expect_identical(getMadratMessage(fname = "calcTauTotal"), list(test2 = list(convertTau = "another test")))
  expect_identical(getMadratMessage(fname = "readTau"), list(test2 = list(convertTau = "another test")))
  expect_identical(getMadratMessage(fname = "convertTau"), list(test2 = list(convertTau = "another test")))
  resetMadratMessages()
  test <- function() {
    putMadratMessage("level", "level 1")
    .tmp <- function() {
      putMadratMessage("level", "level 2")
      putMadratMessage("level", "level 1 again", fname = -2, add = TRUE)
    }
    .tmp()
  }
  test()
  expect_identical(getMadratMessage(), list(level = list(test = c("level 1", "level 1 again"), .tmp = "level 2")))
  expect_silent(resetMadratMessages(fname = ".tmp"))
  expect_identical(getMadratMessage(), list(level = list(test = c("level 1", "level 1 again"))))
  expect_silent(resetMadratMessages("level"))
  expect_null(getMadratMessage())
})

test_that("get/putMadratMessage work in combination with caching", {
  localConfig(globalenv = TRUE, .verbose = FALSE)
  resetMadratMessages()

  calcMessage1 <- function() {
    putMadratMessage("example", "message 1")
    return(list(x = as.magpie(1), description = "-", unit = "-"))
  }
  calcMessage2 <- function() {
    putMadratMessage("example", "message 2")
    return(list(x = as.magpie(2), description = "-", unit = "-"))
  }
  calcMessage3 <- function() {
    x <- calcOutput("Message1", aggregate = FALSE)
    putMadratMessage("example", "message 3")
    return(list(x = as.magpie(3), description = "-", unit = "-"))
  }
  globalassign("calcMessage1", "calcMessage2", "calcMessage3")

  tmp <- calcOutput("Message2", aggregate = FALSE)
  expect_identical(getMadratMessage(), list(example = list(calcMessage2 = "message 2")))
  tmp <- calcOutput("Message3", aggregate = FALSE)
  expect_identical(getMadratMessage(), list(example = list(calcMessage2 = "message 2", calcMessage1 = "message 1",
                                                           calcMessage3 = "message 3")))
  options(madratMessage = NULL)
  expect_null(getMadratMessage())
  tmp <- calcOutput("Message3", aggregate = FALSE)
  expect_identical(getMadratMessage(), list(example = list(calcMessage1 = "message 1",
                                                           calcMessage3 = "message 3")))
  resetMadratMessages()
})
