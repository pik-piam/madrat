test_that("compareData works", {
  testdir <- file.path(withr::local_tempdir(), "test")
  dir.create(testdir)
  withr::defer({
    unlink(testdir, recursive = TRUE)
  })
  write.magpie(magclass::maxample("pop"), file.path(testdir, "pop.cs4"))
  write.magpie(magclass::maxample("animal"), file.path(testdir, "animal.mz"))
  writeLines("Test", file.path(testdir, "test.txt"))
  expectation <- "[OK 2 | DIFF 0 | SKIP 1 | MISS 0]"
  expect_message(compareData(testdir, testdir), expectation, class = "fixed")
})

# Two empty magpie objects (all cells zeroed) sharing one region/year/names
# layout, ready for the caller to overwrite specific cells with old/new pairs.
.compareDataTestPair <- function(names) {
  tDir <- withr::local_tempdir(.local_envir = parent.frame())
  oldDir <- file.path(tDir, "old")
  newDir <- file.path(tDir, "new")
  dir.create(oldDir)
  dir.create(newDir)

  m <- magclass::new.magpie(cells_and_regions = c("AAA.1", "AAA.2"), years = 2000, names = names)
  old <- m
  new <- m
  old[is.na(old)] <- 0
  new[is.na(new)] <- 0

  list(oldDir = oldDir, newDir = newDir, old = old, new = new)
}

test_that("compareData detailed classifies .mz differences into buckets", {
  p <- .compareDataTestPair(c("noise", "zeroflip", "genuine", "namismatch", "exact"))
  oldDir <- p$oldDir
  newDir <- p$newDir
  old <- p$old
  new <- p$new

  u <- 2^(floor(log2(100)) - 23) # float32 ULP at magnitude 100
  old["AAA.1", 2000, "noise"] <- 100
  new["AAA.1", 2000, "noise"] <- 100 + 2 * u        # within 2 ULP -> float32-ULP noise
  old["AAA.1", 2000, "zeroflip"] <- 0
  new["AAA.1", 2000, "zeroflip"] <- 1e-9            # dust vs exact zero -> zero-flip
  old["AAA.1", 2000, "genuine"] <- 100
  new["AAA.1", 2000, "genuine"] <- 101              # 1% off -> genuine
  old["AAA.1", 2000, "namismatch"] <- NA
  new["AAA.1", 2000, "namismatch"] <- 5             # NA vs number -> NA-mismatch
  old["AAA.1", 2000, "exact"] <- 42
  new["AAA.1", 2000, "exact"] <- 42                 # identical -> not counted

  write.magpie(old, file.path(oldDir, "animal.mz"))
  write.magpie(new, file.path(newDir, "animal.mz"))

  plain <- compareData(oldDir, newDir, detailed = FALSE)
  detailed <- compareData(oldDir, newDir, detailed = TRUE)

  # detailed must never change the OK/DIFF/SKIP/MISS verdict
  expect_identical(plain[c("ok", "diff", "skip", "miss")], detailed[c("ok", "diff", "skip", "miss")])

  st <- detailed$details[["animal.mz"]]
  expect_equal(st$n, 10)
  expect_equal(st$nNoise, 1)
  expect_equal(st$nZeroFlip, 1)
  expect_equal(st$nGenuine, 1)
  expect_equal(st$nNAmismatch, 1)
  expect_equal(st$nDiff, 4)
})

test_that("compareData detailed uses reassoc-noise (not float32-ULP) for non-.mz formats", {
  p <- .compareDataTestPair(c("noise", "genuine"))
  oldDir <- p$oldDir
  newDir <- p$newDir
  old <- p$old
  new <- p$new

  old["AAA.1", 2000, "noise"] <- 100
  new["AAA.1", 2000, "noise"] <- 100 * (1 + 1e-10)  # below reassocTol -> noise
  old["AAA.1", 2000, "genuine"] <- 100
  new["AAA.1", 2000, "genuine"] <- 100 * (1 + 1e-3) # above reassocTol -> genuine

  write.magpie(old, file.path(oldDir, "x.rds"))
  write.magpie(new, file.path(newDir, "x.rds"))

  detailed <- compareData(oldDir, newDir, detailed = TRUE)
  st <- detailed$details[["x.rds"]]
  expect_equal(st$nNoise, 1)
  expect_equal(st$nGenuine, 1)
})
