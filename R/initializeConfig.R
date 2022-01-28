#' initializeConfig
#'
#' Checks whether configuration already has been set. If not, it will be initialized
#' with default settings or (if available) system settings.
#'
#' @param verbose boolean deciding whether status information/updates should be shown or not
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMainfolder}}, \code{\link{getConfig}}, \code{\link{setConfig}}
#'
initializeConfig <- function(verbose = TRUE) {
  # check whether config has not been initialized yet
  # and initialize it (otherwise do nothing)
  if (is.null(getOption("madrat_cfg"))) {
    if (verbose) message("\nInitialize madrat config with default settings..")

    cfg <- list(regionmapping        = "regionmappingH12.csv",
                extramappings        = NULL,
                packages             = "madrat",
                globalenv            = FALSE,
                verbosity            = 1,
                mainfolder           = getMainfolder(verbose = verbose),
                sourcefolder         = NA,
                cachefolder          = NA,
                mappingfolder        = NA,
                outputfolder         = NA,
                bundlefolder         = NA,
                nolabels             = NULL,
                forcecache           = FALSE,
                ignorecache          = NULL,
                cachecompression     = "gzip",
                hash                 = "xxhash32",
                diagnostics          = FALSE,
                debug                = FALSE,
                maxLengthLogMessage = 200)
     options(madrat_cfg = cfg) # nolint
     if (verbose) {
      message(paste(paste0("    ", names(cfg)), cfg, sep = " = ", collapse = "\n"))
      message("..done!\n")
     }
  }
}
