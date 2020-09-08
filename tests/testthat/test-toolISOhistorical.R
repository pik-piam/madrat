context("toolISOhistorical")

test_that("Historic data is properly translated", {
  setConfig(.verbose = FALSE)
  newcountries <- c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ",
                    "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB")
  a <- new.magpie(c("SUN",newcountries),1990:1992)
  a["SUN",1990,] <- 240
  a[-1,1991:1992, ] <- rep(1:15,2)
  a <- toolISOhistorical(a)
  expect_identical(as.vector(a[,1990,]), 2*(1:15))
  expect_identical(getRegions(a), newcountries)
})
