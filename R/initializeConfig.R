#' initializeConfig
#' 
#' Checks whether configuration already has been set. If not, it will be initialized
#' with default settings or (if available) system settings.
#' 
#' @param verbose boolean deciding whether status information/updates should be shown or not
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMainfolder}}, \code{\link{getConfig}}, \code{\link{setConfig}}
#' 
initializeConfig <- function(verbose=TRUE) {
  # check whether config has not been initialized yet
  # and initialize it (otherwise do nothing)
  if(is.null(getOption("madrat_cfg"))) {
    if(verbose) base::cat("\nInitialize madrat config with default settings..\n")
    
    cfg <- list(regionmapping = "regionmappingH12.csv",
                packages      = "madrat",
                globalenv     = FALSE,
                verbosity     = 1,
                enablecache   = TRUE,
                mainfolder    = getMainfolder(verbose=verbose),
                sourcefolder  = NA,
                cachefolder   = NA,
                mappingfolder = NA,
                outputfolder  = NA,
                pop_threshold = 10^6,
                forcecache    = FALSE,
                ignorecache = NULL,
                delete_cache  = TRUE,
                diagnostics   = FALSE,
                nocores=1)
     options(madrat_cfg = cfg)
     if(verbose) {
      base::cat(paste(paste0("    ",names(cfg)),cfg,sep=" = ",collapse="\n"))
      base::cat("\n..done!\n\n")
     }
  }   
}

