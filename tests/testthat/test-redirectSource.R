test_that("redirectSource writes to config as intended", {
  withr::local_dir(withr::local_tempdir())

  expect_error(redirectSource("foo", target = "foo2"), "No such file or directory|cannot find the file")
  dir.create("foo2")
  dir.create("foo3")
  dir.create("example2")
  expect_identical(redirectSource("foo", target = "foo2"), normalizePath("foo2"))
  expect_identical(redirectSource("foo", target = "foo3"), normalizePath("foo3"))
  expect_identical(redirectSource("example", target = "example2"), normalizePath("example2"))
  expect_identical(getConfig("redirections"),
                   list(foo = normalizePath("foo3"), example = normalizePath("example2")))
  expect_null(redirectSource("foo", target = NULL))
  expect_identical(getConfig("redirections"), list(example = normalizePath("example2")))
})

test_that("redirectSource works", {
  localConfig(sourcefolder = withr::local_tempdir())
  dir.create(file.path(getConfig("sourcefolder"), "Example"))
  writeLines("123", file.path(getConfig("sourcefolder"), "Example", "Example.txt"))
  target <- file.path(withr::local_tempdir(), "Example.txt")
  writeLines("456", target)
  target2 <- file.path(withr::local_tempdir(), "Example.txt")
  writeLines("456", target2)

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

  redirectSource("Example", target = target2)
  expect_identical(as.vector(readSource("Example")), 456)
  expect_identical(fingerprint("readExample", packages = "madrat"), nfp)

  # ensure same fingerprint after redirecting to the same target again
  redirectSource("Example", target = target)
  expect_identical(as.vector(readSource("Example")), 456)
  expect_identical(fingerprint("readExample", packages = "madrat"), nfp)
})

test_that("redirectSource symlinks other files", {
  sourceFolder <- withr::local_tempdir()
  localConfig(sourcefolder = sourceFolder)
  dir.create(file.path(sourceFolder, "Example"))
  writeLines("123", file.path(sourceFolder, "Example", "Example.txt"))
  writeLines("789", file.path(sourceFolder, "Example", "Example2.txt"))
  target <- file.path(withr::local_tempdir(), "Example.txt")
  writeLines("456", target)

  readExample <- function() {
    return(as.magpie(as.numeric(paste0(readLines("Example.txt"), readLines("Example2.txt")))))
  }
  globalassign("readExample")

  expect_identical(as.vector(readSource("Example")), 123789)
  redirectSource("Example", target = target)
  expect_identical(as.vector(readSource("Example")), 456789)
  redirectSource("Example", target = target, linkOthers = FALSE)
  expect_error({
    expect_warning({
      readSource("Example")
    }, "cannot open file 'Example2.txt': No such file or directory", fixed = TRUE)
  }, "cannot open the connection")
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
    redirectSource("tau", target = "tau2", local = FALSE)
    expect_identical(getConfig("redirections"), list(tau = normalizePath("tau2")))
  }
  f2()
  expect_identical(getConfig("redirections"), list(tau = normalizePath("tau2")))
})

test_that("redirect target can be files", {
  localConfig(redirections = list(), sourcefolder = withr::local_tempdir())
  dir.create(getSourceFolder("Example", subtype = NULL))
  withr::local_dir(withr::local_tempdir())
  writeLines("123", "Example.txt")
  writeLines("456", "Example2.txt")
  readExample <- function() {
    return(as.magpie(as.numeric(paste0(readLines("Example.txt"), readLines("Example2.txt")))))
  }
  globalassign("readExample")

  f <- function(local) {
    redirectSource("Example", target = c("Example.txt", "Example2.txt"), local = local)
    sourceFolder <- normalizePath(getSourceFolder("Example", subtype = NULL))
    expect_true(sourceFolder != normalizePath("."))
    expect_setequal(dir(sourceFolder), c("Example.txt", "Example2.txt"))
    expect_identical(as.vector(readSource("Example")), 123456)
    return(sourceFolder)
  }

  sourceFolder <- f(local = TRUE)
  # temporary sourcefolder should only exist while executing f, after f returned it should be deleted
  expect_true(!dir.exists(sourceFolder))

  sourceFolder <- f(local = FALSE)
  # not local, so temporary sourcefolder should exist until the R session closes
  expect_true(dir.exists(sourceFolder))

  readExample <- function() {
    return(as.magpie(as.numeric(paste0(readLines("Example.txt")))))
  }
  globalassign("readExample")

  redirectSource("Example", target = "Example.txt")
  sourceFolder <- normalizePath(getSourceFolder("Example", subtype = NULL))
  expect_true(sourceFolder != normalizePath("."))
  expect_setequal(dir(sourceFolder), c("Example.txt", "Example2.txt"))
  expect_identical(as.vector(readSource("Example")), 123)

  readExample <- function() {
    return(as.magpie(as.numeric(paste0(readLines("some/subfolder/Example.txt")))))
  }
  globalassign("readExample")

  redirectSource("Example", target = c(`some/subfolder/Example.txt` = "Example.txt"))
  sourceFolder <- normalizePath(getSourceFolder("Example", subtype = NULL))
  expect_true(sourceFolder != normalizePath("."))
  expect_setequal(dir(sourceFolder), c("some", "Example.txt", "Example2.txt"))
  expect_identical(as.vector(readSource("Example")), 123)

  redirectSource("Example", target = c("some/subfolder/" = "Example.txt"))
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

test_that("redirectSource symlinks all other files", {
  localConfig(sourcefolder = withr::local_tempdir(), cachefolder = withr::local_tempdir())
  sourceFolder <- getSourceFolder("Example", subtype = NULL)
  dir.create(sourceFolder)
  for (p in c("A/a.txt",
              "B/B1/b.txt", "B/B2/b.txt", "B/B2/.hidden", "B/B2/B21/b.txt",
              "C/c.txt",
              "x.txt")) {
    dir.create(file.path(sourceFolder, dirname(p)), showWarnings = FALSE, recursive = TRUE)
    writeLines(p, file.path(sourceFolder, p))
  }

  target <- file.path(withr::local_tempdir(), "redirected.txt")
  writeLines("redirected", target)

  readExample <- function() {
    x <- as.magpie(1)
    attr(x, "content") <- readLines("x.txt")
    return(x)
  }
  globalassign("readExample")
  expect_identical(attr(readSource("Example"), "content"), "x.txt")
  redirectedSourceFolder <- redirectSource("Example", target = c(`x.txt` = target))
  expect_identical(attr(readSource("Example"), "content"), "redirected")
  expect_identical(readLines(file.path(redirectedSourceFolder, "x.txt")), readLines(target))
  expect_identical(dir(file.path(redirectedSourceFolder, "A")), dir(file.path(sourceFolder, "A")))
  expect_identical(dir(file.path(redirectedSourceFolder, "B")), dir(file.path(sourceFolder, "B")))
  expect_identical(dir(file.path(redirectedSourceFolder, "C")), dir(file.path(sourceFolder, "C")))

  readExample <- function() {
    x <- as.magpie(1)
    attr(x, "content") <- readLines("B/B2/b.txt")
    return(x)
  }
  globalassign("readExample")
  redirectSource("Example", target = NULL)
  expect_identical(attr(readSource("Example"), "content"), "B/B2/b.txt")
  redirectedSourceFolder <- redirectSource("Example", target = c(`B/B2/b.txt` = target))
  expect_identical(attr(readSource("Example"), "content"), "redirected")
  expect_identical(readLines(file.path(redirectedSourceFolder, "x.txt")), readLines(file.path(sourceFolder, "x.txt")))
  expect_identical(dir(file.path(redirectedSourceFolder, "A")), dir(file.path(sourceFolder, "A")))
  expect_identical(dir(file.path(redirectedSourceFolder, "B/B1")), dir(file.path(sourceFolder, "B/B1")))
  expect_identical(readLines(file.path(redirectedSourceFolder, "B/B2/b.txt")), readLines(target))
  expect_identical(readLines(file.path(redirectedSourceFolder, "B/B2/.hidden")),
                   readLines(file.path(sourceFolder, "B/B2/.hidden")))
  expect_identical(dir(file.path(redirectedSourceFolder, "B/B2/B21")),
                   dir(file.path(sourceFolder, "B/B2/B21")))
  expect_identical(dir(file.path(redirectedSourceFolder, "C")), dir(file.path(sourceFolder, "C")))
})
