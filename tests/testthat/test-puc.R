test_that("puc creation works", {
  skip_on_cran()
  localMockedTauDownload()
  retrieveData("example", rev = 42, extra = "test1")
  expect_true(dir.exists(getConfig("pucfolder")))
  withr::local_dir(getConfig("pucfolder"))
  expect_true(file.exists("rev42_extra_example_tag.puc"))
  untar("rev42_extra_example_tag.puc")
  expect_true(length(Sys.glob("calcTauTotal*.rds")) == 1)
  cfg <- readRDS("config.rds")
  expect_identical(cfg$package, "madrat")
  expect_identical(cfg$pucArguments, "extra")
  expect_identical(cfg$args, list(model = "example", rev = 42, dev = "", cachetype = "def",
                                  puc = TRUE, strict = FALSE, extra = "test1"))
  expect_error(pucAggregate("rev42_extra_example_tag.puc", bla = "blub"), "cannot be changed in the given puc")
  expect_message(pucAggregate("rev42_extra_example_tag.puc", extra = "blub", regionmapping = "regionmappingH12.csv",
                              renv = FALSE), "Run calcOutput")
  expect_message(pucAggregate("rev42_extra_example_tag.puc", extra = "blub", regionmapping = "regionmappingH12.csv",
                              renv = FALSE), "already available")
  expect_true(file.exists(file.path(getConfig("outputfolder"), "rev42_h12_7a5441e5_example_customizable_tag.tgz")))

  expect_message(retrieveData("example", rev = 42, extra = "test2", renv = FALSE), "Run pucAggregate")
  expect_true(file.exists(file.path(getConfig("outputfolder"), "rev42_h12_5f3d77a0_example_customizable_tag.tgz")))
})

# Starts a sub-process which acquires the lock for the given puc via .withLockedPuc and holds it
# until releaseFile shows up. It prints "acquired" once it is inside the critical section and
# "released" once it has left it again.
.startLockHolder <- function(pucName, releaseFile) {
  callr::r_bg(function(madratConfig, pucName, releaseFile) {
    pkgload::load_all("../..")
    do.call(madrat::setConfig, madratConfig)

    madrat:::.withLockedPuc(pucName, function() {
      cat("acquired\n")
      flush(stdout())
      while (!file.exists(releaseFile)) {
        Sys.sleep(0.05)
      }
    })
    cat("released\n")
    flush(stdout())
  }, args = list(madratConfig = getConfig(), pucName = pucName, releaseFile = releaseFile))
}

test_that(".withLockedPuc grants exclusive access across processes", {
  skip_on_cran()
  # "../.." (above) only locates the package source when this session has madrat loaded via
  # pkgload::load_all() from that path. Under covr::package_coverage() tests run against a
  # library()-installed copy in an unrelated temp dir, so the sub-process fails to load madrat.
  skip_on_covr()

  pucName <- "testlock_example.puc"
  releaseFile <- file.path(withr::local_tempdir(), "release")

  holder <- .startLockHolder(pucName, releaseFile)
  withr::defer(holder$kill())
  holderLog <- processLog(holder)

  # From here on the sub-process provably holds the lock, so no waiting heuristics are needed below
  holderLog$waitFor("acquired")

  # filelock locks are shared within a process, so the lock has to be probed from this process, not
  # from the one holding it. timeout = 0 turns the probe into a non-blocking try.
  lockPath <- madrat:::.pucLockPath(pucName)
  blockedLock <- filelock::lock(lockPath, timeout = 0)
  if (!is.null(blockedLock)) {
    filelock::unlock(blockedLock)
  }
  expect_null(blockedLock)

  # locking is per puc, an unrelated puc must not be blocked
  otherLock <- filelock::lock(madrat:::.pucLockPath("othertestlock_example.puc"), timeout = 0)
  expect_false(is.null(otherLock))
  if (!is.null(otherLock)) {
    filelock::unlock(otherLock)
  }

  file.create(releaseFile)
  holderLog$waitFor("released")
  holder$wait()
  expect_identical(holder$get_exit_status(), 0L)

  releasedLock <- filelock::lock(lockPath, timeout = 5000)
  expect_false(is.null(releasedLock))
  if (!is.null(releasedLock)) {
    filelock::unlock(releasedLock)
  }
})

test_that("a second .withLockedPuc caller waits until the lock is released", {
  skip_on_cran()
  skip_on_covr() # see the comment on the previous test

  pucName <- "testlock_waiting_example.puc"
  releaseFile <- file.path(withr::local_tempdir(), "release")

  holder <- .startLockHolder(pucName, releaseFile)
  withr::defer(holder$kill())
  holderLog <- processLog(holder)
  holderLog$waitFor("acquired")

  waiter <- callr::r_bg(function(madratConfig, pucName) {
    pkgload::load_all("../..")
    do.call(madrat::setConfig, madratConfig)

    # "requesting" is printed immediately before the lock is requested, so that the only thing
    # happening between the two markers is the blocking filelock call itself
    cat("requesting\n")
    flush(stdout())
    madrat:::.withLockedPuc(pucName, function() {
      cat("entered\n")
      flush(stdout())
    })
  }, args = list(madratConfig = getConfig(), pucName = pucName))
  withr::defer(waiter$kill())
  waiterLog <- processLog(waiter)

  waiterLog$waitFor("requesting")
  Sys.sleep(1) # settle window, the waiter is a single filelock call away from the critical section
  expect_false(waiterLog$contains("entered"))

  file.create(releaseFile)
  waiterLog$waitFor("entered") # fails after the timeout if the waiter never gets the lock
  waiter$wait()
  holder$wait()
  expect_identical(waiter$get_exit_status(), 0L)
  expect_identical(holder$get_exit_status(), 0L)
})

test_that("retrieveData locks puc creation and puc reading", {
  skip_on_cran()
  localMockedTauDownload()

  lockedNames <- character(0)
  originalWithLockedPuc <- madrat:::.withLockedPuc # has to be captured before mocking, else recursion
  local_mocked_bindings(.withLockedPuc = function(pucName, fn) {
    lockedNames <<- c(lockedNames, pucName)
    originalWithLockedPuc(pucName, fn)
  }, .package = "madrat")

  # creates the puc
  retrieveData("example", rev = 46, renv = FALSE)
  expect_identical(lockedNames, "rev46_extra_example_tag.puc")

  # a different value for the puc argument "extra" reuses the same puc via pucAggregate
  expect_message(retrieveData("example", rev = 46, extra = "other", renv = FALSE), "Run pucAggregate")
  expect_identical(lockedNames, rep("rev46_extra_example_tag.puc", 2))
})
