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
