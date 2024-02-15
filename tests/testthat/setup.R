# set madrat mainfolder and options before running tests, cleanup after all tests are finished

previousMadratConfig <- getOption("madrat_cfg")
withr::defer({
  options(madrat_cfg = previousMadratConfig) # nolint
}, envir = testthat::teardown_env())

setConfig(mainfolder = withr::local_tempdir(.local_envir = testthat::teardown_env()),
          globalenv = TRUE,
          redirections = list(),
          .verbose = FALSE)
withr::local_options(madrat_codelabels = NULL,
                     MadratCache = NULL,
                     renv.verbose = FALSE,
                     magclass_expand_version = NULL,
                     magclass_sizeLimit = NULL,
                     .local_envir = testthat::teardown_env())

withr::local_envvar(LANGUAGE = "EN",
                    .local_envir = testthat::teardown_env())

globalassign <- function(...) {
  withr::defer({
    # suppress warning if object does not exist in .GlobalEnv
    suppressWarnings(rm(list = c(...), envir = .GlobalEnv))
  }, envir = parent.frame())
  for (x in c(...)) {
    assign(x, eval.parent(parse(text = x)), .GlobalEnv)
  }
}
