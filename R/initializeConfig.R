#' initializeConfig
#'
#' Checks whether configuration already has been set. If not, it will be initialized
#' with default settings or (if available) system settings. All madrat folders (see
#' \code{\link{setConfig}} for documentation which folders are available) will be set
#' to the system environment variables MADRAT_SOURCEFOLDER, MADRAT_CACHEFOLDER, etc.
#' if they exist, NA otherwise. NA means subfolders of the mainfolder are used.
#'
#' @param verbose boolean deciding whether status information/updates should be shown or not
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMainfolder}}, \code{\link{getConfig}}, \code{\link{setConfig}}
#'
initializeConfig <- function(verbose = TRUE) {
  # check whether config has not been initialized yet
  # and initialize it (otherwise do nothing)
  if (is.null(getOption("madrat_cfg"))) {
    if (verbose) {
      message("\nInitializing madrat config with default settings")
    }

    cfg <- list(regionmapping        = "regionmappingH12.csv",
                extramappings        = NULL,
                packages             = "madrat",
                globalenv            = FALSE,
                verbosity            = 1,
                mainfolder           = getMainfolder(verbose = verbose),
                sourcefolder         = Sys.getenv("MADRAT_SOURCEFOLDER", unset = NA),
                cachefolder          = Sys.getenv("MADRAT_CACHEFOLDER", unset = NA),
                mappingfolder        = Sys.getenv("MADRAT_MAPPINGFOLDER", unset = NA),
                outputfolder         = Sys.getenv("MADRAT_OUTPUTFOLDER", unset = NA),
                pucfolder            = Sys.getenv("MADRAT_PUCFOLDER", unset = NA),
                tmpfolder            = Sys.getenv("MADRAT_TMPFOLDER", unset = NA),
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
