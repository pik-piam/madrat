# SPDX-FileCopyrightText: 2025 Potsdam Institute for Climate Impact Research (PIK)
# SPDX-License-Identifier: BSD-2-Clause

# set madrat mainfolder and options before running tests, cleanup after all tests are finished

# enable sourcing this file during interactive development of tests
teardownEnv <- tryCatch(testthat::teardown_env(), error = function(e) .GlobalEnv)

previousMadratConfig <- getOption("madrat_cfg")
withr::defer({
  options(madrat_cfg = previousMadratConfig) # nolint
}, envir = teardownEnv)

setConfig(mainfolder = withr::local_tempdir(.local_envir = teardownEnv),
          globalenv = TRUE,
          redirections = list(),
          .verbose = FALSE)
withr::local_options(madrat_codelabels = NULL,
                     MadratCache = NULL,
                     renv.verbose = FALSE,
                     magclass_expand_version = NULL,
                     magclass_sizeLimit = NULL,
                     .local_envir = teardownEnv)

withr::local_envvar(LANGUAGE = "EN",
                    .local_envir = teardownEnv)

globalassign <- function(...) {
  withr::defer({
    # suppress warning if object does not exist in .GlobalEnv
    suppressWarnings(rm(list = c(...), envir = .GlobalEnv))
  }, envir = parent.frame())
  for (x in c(...)) {
    assign(x, eval.parent(parse(text = x)), .GlobalEnv)
  }
}
