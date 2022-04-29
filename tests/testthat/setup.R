# set madrat mainfolder and options before running tests, cleanup after all tests are finished
local({
  previousMadratConfig <- getOption("madrat_cfg")
  setConfig(mainfolder = withr::local_tempdir(.local_envir = teardown_env()), .verbose = FALSE)
  withr::local_options(madrat_codelabels = NULL, MadratCache = NULL, renv.verbose = FALSE,
                       magclass_expand_version = NULL, magclass_sizeLimit = NULL, .local_envir = teardown_env())

  withr::defer({
    options(madrat_cfg = previousMadratConfig) # nolint
  }, envir = teardown_env())
})
