test_that("localRedirect works", {
  localConfig(redirections = list())
  withr::local_dir(withr::local_tempdir())

  expect_error(localRedirect("tau", "tau2"), "No such file or directory")
  dir.create("tau2")
  dir.create("tau3")
  dir.create("example2")
  expect_identical(localRedirect("tau", "tau2"), list(tau = normalizePath("tau2")))
  expect_identical(localRedirect("tau", "tau3"), list(tau = normalizePath("tau3")))
  expect_identical(localRedirect("example", "example2"),
                   list(tau = normalizePath("tau3"), example = normalizePath("example2")))
  expect_identical(localRedirect("tau", NULL), list(example = normalizePath("example2")))

  # TODO:
  # create dummy read function
  # create temp sourcefolder
  # run read function to check it reads of temp sourcefolder
  # redirect, run read again, check it got redirected
})
