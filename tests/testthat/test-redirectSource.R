test_that("redirectSource writes to config as intended", {
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
})

test_that("redirectSource works", {
  localConfig(sourcefolder = withr::local_tempdir())
  dir.create(file.path(getConfig("sourcefolder"), "Example"))
  writeLines("123", file.path(getConfig("sourcefolder"), "Example", "Example.txt"))
  target <- file.path(withr::local_tempdir(), "Example.txt")
  writeLines("456", target)

  readExample <- function() {
    return(as.magpie(as.numeric(readLines("Example.txt"))))
  }
  globalassign("readExample")

  expect_identical(as.vector(readSource("Example")), 123)
  fp <- fingerprint("readExample", packages = "madrat")

  # ensure fingerprint is different after redirecting
  redirectSource("Example", target = target)
  expect_identical(as.vector(readSource("Example")), 456)
  nfp <- fingerprint("readExample", packages = "madrat")
  expect_true(nfp != fp)

  # ensure original fingerprint after resetting redirection
  redirectSource("Example", target = NULL)
  expect_identical(as.vector(readSource("Example")), 123)
  expect_identical(fingerprint("readExample", packages = "madrat"), fp)

  # ensure same fingerprint after redirecting to the same target again
  redirectSource("Example", target = target)
  expect_identical(as.vector(readSource("Example")), 456)
  expect_identical(fingerprint("readExample", packages = "madrat"), nfp)
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

test_that("caching works with redirectSource", {
  localConfig(sourcefolder = withr::local_tempdir(), cachefolder = withr::local_tempdir())
  dir.create(file.path(getConfig("sourcefolder"), "Example"))
  writeLines("123", file.path(getConfig("sourcefolder"), "Example", "Example.txt"))
  target <- file.path(withr::local_tempdir(), "Example.txt")
  writeLines("456", target)

  readExample <- function() {
    return(as.magpie(as.numeric(readLines("Example.txt"))))
  }
  globalassign("readExample")

  expect_message(readSource("Example"), "writing cache")
  expect_message(readSource("Example"), "loading cache")

  redirectSource("Example", target = target)
  expect_message(readSource("Example"), "writing cache")
  expect_message(readSource("Example"), "loading cache")

  redirectSource("Example", target = NULL)
  expect_message(readSource("Example"), "loading cache")

  redirectSource("Example", target = target)
  expect_message(readSource("Example"), "loading cache")
})
