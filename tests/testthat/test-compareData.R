test_that("compareData works", {
  td <- tempdir()
  testdir <- file.path(td, "test")
  dir.create(testdir)
  on.exit(unlink(testdir, recursive = TRUE))
  write.magpie(maxample("pop"), file.path(testdir, "pop.cs4"))
  write.magpie(maxample("animal"), file.path(testdir, "animal.mz"))
  writeLines("Test", file.path(testdir, "test.txt"))
  expectation <- "[OK 2 | DIFF 0 | SKIP 1 | MISS 0]"
  expect_message(compareData(testdir, testdir), expectation, class = "fixed")
})
