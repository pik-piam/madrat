test_that("puc creation works", {
  skip_on_cran()
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

test_that("puc creation is thread-safe", {
  skip_on_cran()

  # Tip for debugging this test:
  # If something breaks in one of the sub-processes, use p1$get_result()
  # to check what happened.

  # Store paths
  madratPkgPath <- pkgload::pkg_path("../..")
  lockDir <- ".locks"

  # Set up utility functions
  .waitForMessage <- function(process, message) {
    while (!(length(messages <- process$read_output_lines()) > 0 && any(messages == message))) {
      Sys.sleep(0.1)
    }
  }

  # Set up PUC-folder as working directory
  withr::local_dir(getConfig("pucfolder"))
  unlink(list.files(full.names = TRUE)) # To ensure no remaining pucs are in there
  unlink(list.files(lockDir, full.names = TRUE)) # To ensure no remaining locks
  dir.create(lockDir)

  # Set up locks
  firstCheckpoint <- filelock::lock(file.path(lockDir, "checkpoint1.lock"))
  secondCheckpoint <- filelock::lock(file.path(lockDir, "checkpoint2.lock"))

  p1 <- callr::r_bg(function(pkgPath, madratConfig) {
    # Set up environment
    pkgload::load_all(pkgPath)
    do.call(madrat::setConfig, madratConfig)
    withr::local_dir(getConfig("pucfolder"))

    # Set up .withLockedPuc wrapper to inject control logic
    # into the passed function
    originalWithLockedPuc <- madrat:::.withLockedPuc
    assignInNamespace(".withLockedPuc", function(pucName, fn) {
      originalWithLockedPuc(pucName, function() {
        if (any(grepl("pucAggregate", deparse(fn), fixed = TRUE))) {
          # Only interested in creation, so we do a quick return for reading the puc.
          return(fn())
        }

        tryCatch({
          cat("ready\n")
          firstCheckpoint <- filelock::lock(file.path(".locks", "checkpoint1.lock"))
          fn()
          cat("fn done\n")
          secondCheckpoint <- filelock::lock(file.path(".locks", "checkpoint2.lock"))
        },
        finally = {
          filelock::unlock(firstCheckpoint)
          filelock::unlock(secondCheckpoint)
        })
      })
    }, ns = "madrat")

    # Start
    madrat::retrieveData("example", rev = 45)
  }, args = list(pkgPath = madratPkgPath, madratConfig = getConfig()))

  # Wait for p1 to signal that it is ready, i.e. it has entered the critical section
  .waitForMessage(p1, "ready")

  # This is a copy of testFunction1 except for the call at the end
  p2 <- callr::r_bg(function(pkgPath, madratConfig) {
    # Set up environment
    pkgload::load_all(pkgPath)
    do.call(madrat::setConfig, madratConfig)
    withr::local_dir(getConfig("pucfolder"))

    # Set up .withLockedPuc wrapper to inject control logic
    # into the passed function
    originalWithLockedPuc <- madrat:::.withLockedPuc
    assignInNamespace(".withLockedPuc", function(pucName, fn) {
      originalWithLockedPuc(pucName, function() {
        if (any(grepl("pucAggregate", deparse(fn), fixed = TRUE))) {
          # Only interested in creation, so we do a quick return for reading the puc.
          return(fn())
        }

        tryCatch({
          fn()
        },
        finally = {})
      })
    }, ns = "madrat")

    # Start
    cat("ready\n")
    madrat::retrieveData("example", rev = 45)
  }, args = list(pkgPath = madratPkgPath, madratConfig = getConfig()))

  # Wait for p2 to signal that it is ready, i.e. it was at the point where it could execute 
  # the critical section (there is no guarantee that it tried getting in yet, if this test
  # is flaky, this is one of the critical spots).
  .waitForMessage(p2, "ready")

  filelock::unlock(firstCheckpoint)

  # Wait for p1 to signal that it is done, i.e. it has executed fn
  .waitForMessage(p1, "fn done")

  Sys.sleep(1) # Give p2 some time to fall asleep (start waiting for the puc lock)

  expect_true(p2$get_status() == "sleeping")
  expect_true(file.exists("rev45_extra_example_tag.puc"))
  unlink("rev45_extra_example_tag.puc")

  filelock::unlock(secondCheckpoint)
  p1$wait()
  p2$wait()

  expect_true(file.exists("rev45_extra_example_tag.puc"))

  expect_false(p1$is_alive())
  expect_false(p2$is_alive())
})
