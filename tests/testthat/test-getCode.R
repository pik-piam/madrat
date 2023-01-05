test_that("getCode works", {
 localConfig(verbosity = 1, .verbose = FALSE)
 expect_silent({
   a <- getCode("madrat")
  })
 flags <- list(pucArguments = list(`madrat:::fullEXAMPLE` = "extra"),
               monitor = list(`madrat:::readTau` = c("madrat:::sysdata$iso_cell",
                                                     "magclass:::ncells")),
               ignore = list(`madrat:::readTau` = "madrat:::toolAggregate"))
 expect_identical(attr(a, "flags"), flags)
 expect_setequal(names(attributes(a)), c("names", "fpool", "hash", "mappings", "flags"))
 calcTauTotal <- function() {
   return(1)
 }
 calcFlagTest <- function() {
   "!# @ignore  testIgnore"
   "!# @ignore  ignoreMore"
   return(1)
 }
 globalassign("calcTauTotal", "calcFlagTest")
 expect_warning(a <- getCode("madrat"), "Duplicate functions")
 expect_setequal(attr(a, "flags")$ignore$calcFlagTest, c("testIgnore", "ignoreMore"))
 rm(list = c("calcTauTotal", "calcFlagTest"), envir = .GlobalEnv)
 expect_null(attr(getCode(NULL, TRUE), "flags"))
})
