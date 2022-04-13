library(testthat)
library(madrat)
setConfig(mainfolder = withr::local_tempdir(), .verbose = FALSE, .local = TRUE)

test_check("madrat")
