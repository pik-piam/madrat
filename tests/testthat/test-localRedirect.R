test_that("localRedirect works", {
  localConfig(redirections = list())
  withr::local_dir(withr::local_tempdir())

  expect_error(localRedirect("tau", target = "tau2"), "No such file or directory")
  dir.create("tau2")
  dir.create("tau3")
  dir.create("example2")
  expect_identical(localRedirect("tau", target = "tau2"), list(tau = normalizePath("tau2")))
  expect_identical(localRedirect("tau", target = "tau3"), list(tau = normalizePath("tau3")))
  expect_identical(localRedirect("example", target = "example2"),
                   list(tau = normalizePath("tau3"), example = normalizePath("example2")))
  expect_identical(localRedirect("tau", target = NULL), list(example = normalizePath("example2")))

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
  localRedirect("Example", target = "Example2")
  expect_identical(as.vector(readSource("Example")), 456)
})

test_that("scope for localRedirect can be set", {
  localConfig(redirections = list())
  withr::local_dir(withr::local_tempdir())
  dir.create("tau2")
  dir.create("tau3")

  f1 <- function() {
    localRedirect("tau", target = "tau2")
    expect_identical(getConfig("redirections"), list(tau = normalizePath("tau2")))
  }
  f()
  expect_identical(getConfig("redirections"), list())
})
