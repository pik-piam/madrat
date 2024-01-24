test_that("redirectSource works", {
  localConfig(redirections = list())
  withr::local_dir(withr::local_tempdir())

  expect_error(redirectSource("tau", target = "tau2"), "No such file or directory")
  dir.create("tau2")
  dir.create("tau3")
  dir.create("example2")
  expect_identical(redirectSource("tau", target = "tau2"), list(tau = normalizePath("tau2")))
  expect_identical(redirectSource("tau", target = "tau3"), list(tau = normalizePath("tau3")))
  expect_identical(redirectSource("example", target = "example2"),
                   list(tau = normalizePath("tau3"), example = normalizePath("example2")))
  expect_identical(redirectSource("tau", target = NULL), list(example = normalizePath("example2")))

  localConfig(redirections = list())
  withr::local_dir(withr::local_tempdir())
  localConfig(sourcefolder = ".")

  dir.create("Example")
  writeLines("123", "Example/Example.txt")
  dir.create("Example2")
  writeLines("456", "Example2/Example.txt")

  readExample <- function() {
    return(as.magpie(as.numeric(readLines("Example.txt"))))
  }
  globalassign("readExample")

  expect_identical(as.vector(readSource("Example")), 123)
  redirectSource("Example", target = "Example2")
  expect_identical(as.vector(readSource("Example")), 456)
})

test_that("scope for redirectSource can be set", {
  localConfig(redirections = list())
  withr::local_dir(withr::local_tempdir())
  dir.create("tau2")
  dir.create("tau3")

  f1 <- function() {
    redirectSource("tau", target = "tau2")
    expect_identical(getConfig("redirections"), list(tau = normalizePath("tau2")))
  }
  f1()
  expect_identical(getConfig("redirections"), list())

  f2 <- function() {
    redirectSource("tau", target = "tau2", .local = FALSE)
    expect_identical(getConfig("redirections"), list(tau = normalizePath("tau2")))
  }
  f2()
  expect_identical(getConfig("redirections"), list(tau = normalizePath("tau2")))
})

test_that("redirect target can be files", {
  localConfig(redirections = list())
  withr::local_dir(withr::local_tempdir())
  writeLines("123", "Example.txt")
  writeLines("456", "Example2.txt")
  readExample <- function() {
    return(as.magpie(as.numeric(paste0(readLines("Example.txt"), readLines("Example2.txt")))))
  }
  globalassign("readExample")

  f <- function(.local) {
    redirectSource("Example", target = c("Example.txt", "Example2.txt"), .local = .local)
    sourceFolder <- normalizePath(getSourceFolder("Example", subtype = NULL))
    expect_true(sourceFolder != normalizePath("."))
    expect_true(setequal(dir(sourceFolder), c("Example.txt", "Example2.txt")))
    expect_identical(as.vector(readSource("Example")), 123456)
    return(sourceFolder)
  }

  sourceFolder <- f(.local = TRUE)
  # temporary sourcefolder should only exist while executing f, after f returned it should be deleted
  expect_true(!dir.exists(sourceFolder))

  sourceFolder <- f(.local = FALSE)
  # not local, so temporary sourcefolder should exist until the R session closes
  expect_true(dir.exists(sourceFolder))

  readExample <- function() {
    return(as.magpie(as.numeric(paste0(readLines("Example.txt")))))
  }
  globalassign("readExample")

  redirectSource("Example", target = "Example.txt")
  sourceFolder <- normalizePath(getSourceFolder("Example", subtype = NULL))
  expect_true(sourceFolder != normalizePath("."))
  expect_identical(dir(sourceFolder), "Example.txt")
  expect_identical(as.vector(readSource("Example")), 123)
})
