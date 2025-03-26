test_that("fingerprinting works as expected", {
  toolTest <- function() {
    this <- 1
    is <- 2
    a <- 3
    test <- 4
    return(paste0(this, is, a, test))
  }
  globalassign("toolTest")
  expect_equivalent(fingerprint("toolTest", packages = "madrat"), "dc0a3502")
  emptyfolder <- paste0(withr::local_tempdir(), "/empty")
  dir.create(emptyfolder, recursive = TRUE, showWarnings = FALSE)
  expect_equal(unname(fingerprintFiles(emptyfolder)), "bc4159c0")
})

test_that("fingerprintFiles works as expected", {
  withr::local_dir(withr::local_tempdir())
  writeLines("this is a test", "test.txt", sep = "")
  expect_identical(fingerprintFiles("test.txt"), c(test.txt = "e495aa95"))

  # need to write into sourcefolder to use hash cache system
  folder <- withr::local_tempdir(tmpdir = file.path(getConfig("sourcefolder")))
  f <- withr::local_tempfile(tmpdir = folder)
  writeLines("this is a test", f, sep = "")
  fp <- fingerprintFiles(f)
  # check result stays the same second time when using hash cache:
  expect_identical(fingerprintFiles(f), fp)

  hashCacheRds <- paste0(getConfig("cachefolder"), "/fileHashCache", basename(folder), ".rds")
  expect_equal(nrow(readRDS(hashCacheRds)), 1)
  expect_identical(colnames(readRDS(hashCacheRds)), c("name", "mtime", "size", "key", "hash"))

  f2 <- withr::local_tempfile(tmpdir = folder)
  writeLines("this is another test", f2, sep = "")

  fingerprintFiles(folder) # this overwrites the hashCacheRds
  expect_equal(nrow(readRDS(hashCacheRds)), 2)
  expect_identical(colnames(readRDS(hashCacheRds)), c("name", "mtime", "size", "key", "hash"))
})

test_that("fingerprinting works for edge cases", {
  localConfig(verbosity = 1, .verbose = FALSE)
  withr::local_dir(withr::local_tempdir())
  writeLines("this is a test", "map.csv", sep = "")
  readFingerprintTest <- function() {
    map <- toolGetMapping("map.csv", where = "local")
    return(1)
  }
  globalassign("readFingerprintTest")
  expect_silent({
    fp <- fingerprint("readFingerprintTest", packages = getConfig("packages"), details = TRUE)
  })
  expect_identical(attr(fp, "details")[-1], c("map.csv" = "59eab5b3", readFingerprintTest = "b5efba0b"))
  expect_null(fingerprintCall("blub"))
})

test_that("empty hash cache file is handled properly", {
  localTempdir <- withr::local_tempdir()
  dir.create(file.path(localTempdir, "something"))
  file.create(file.path(localTempdir, paste0("fileHashCache", basename(localTempdir), ".rds")))
  localConfig(sourcefolder = localTempdir, cachefolder = localTempdir)
  expect_warning(fingerprintFiles(localTempdir),
                 "Ignoring corrupt hashCacheFile: Error in readRDS(hashCacheFile) : error reading from connection",
                 fixed = TRUE)
})

test_that("fingerprinting works with control flags", {
  localConfig(verbosity = 1, .verbose = FALSE)
  readData <- function() {
    return(1)
  }
  readData2 <- function() {
    return(2)
  }
  calcExample2 <- function() {
    a <- readSource("Data")
    if (FALSE) b <- readSource("Data2")
  }
  globalassign("readData", "readData2", "calcExample2")
  fpExpected <- structure("fb5e6e40",
    call = "calcExample2",
    details = c(
      calcExample2 = "8386790d",
      readData = "667d53aa",
      readData2 = "041bef7f"
    )
  )
  expect_identical(fingerprint("calcExample2", details = TRUE, packages = "madrat"), fpExpected)
  readData3 <- function() {
    return(3)
  }
  calcExample2 <- function() {
    "!# @monitor readData3"
    "!# @ignore  readData2"
    a <- readSource("Data")
    if (FALSE) b <- readSource("Data2")
  }
  calcExample3 <- function() {
    a <- calcOutput("Example2", aggregate = FALSE)
  }
  calcExample4 <- function() {
    "!# @monitor readData2"
    "!# @ignore  readData2"
  }
  globalassign("readData3", "calcExample2", "calcExample3", "calcExample4")

  fp2Expected <- structure("cef10efc",
    call = "calcExample2",
    details = c(
      calcExample2 = "ded5ab1f",
      readData = "667d53aa", readData3 = "56324bcf"
    )
  )
  fp3Expected <- structure("7ba6d631",
    call = "calcExample3",
    details = c(
      calcExample2 = "ded5ab1f",
      calcExample3 = "94819ad1",
      readData = "667d53aa",
      readData2 = "041bef7f",
      readData3 = "56324bcf"
    )
  )
  fp4Expected <- structure("bd5bdbf9",
    call = "calcExample4",
    details = c(
      calcExample4 = "9aad0909",
      readData2 = "041bef7f"
    )
  )
  expect_identical(fingerprint("calcExample2", details = TRUE, packages = "madrat"), fp2Expected)
  expect_identical(fingerprint("calcExample3", details = TRUE, packages = "madrat"), fp3Expected)
  expect_identical(fingerprint("calcExample4", details = TRUE, packages = "madrat"), fp4Expected)
})
