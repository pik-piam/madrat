test_that("robustOrder sorts locale independent", {
  x <- c("a", "b", "C")
  withr::local_locale(LC_COLLATE = "C")
  expect_equal(order(x), c(3, 1, 2))
  expect_equal(robustOrder(x), c(3, 1, 2))

  if (!identical(Sys.info()[["sysname"]], "Windows")) {
    allLocales <- system("locale -a", intern = TRUE)
    expect_true(all(vapply(
      allLocales[seq_len(min(20, length(allLocales)))],
      function(availableLocale) {
        withr::local_locale(LC_COLLATE = availableLocale)
        return(isTRUE(all.equal(robustOrder(x), c(3, 1, 2))))
      },
      logical(1)
    )))
  }
})

test_that("robustOrder can deal with non-UTF-8 strings", {
  x <- c("2Mor\xe9e et al_2013.pdf", "1Mor\xe9e et al_2013.pdf")
  expect_error(order(x, method = "radix"), "Character encoding must be UTF-8, Latin-1 or bytes")
  expect_equal(robustOrder(x), c(2, 1))
  expect_equal(robustSort(x), c("1Mor\xe9e et al_2013.pdf", "2Mor\xe9e et al_2013.pdf"))

  expect_equal(robustSort(c(5, 3, 2, 4), decreasing = TRUE), 5:2)
  expect_equal(robustOrder(c(5, 2, 2, 4), c(0, 1, 0, 17)), c(3, 2, 4, 1))
  expect_equal(robustSort(vector(mode = "character", length = 0)), vector(mode = "character", length = 0))
  expect_equal(robustOrder(vector(mode = "character", length = 0)), vector(mode = "integer", length = 0))
})
