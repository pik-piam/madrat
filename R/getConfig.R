#' getConfig
#' 
#' This function returns the madrat config which is currently loaded. If no
#' configuration has been loaded so far the configuration will be initialized
#' with default settings or system settings (if available).
#' 
#' 
#' @param option The option for which the setting should be returned. If set to
#' NULL all options are returned.
#' @param raw If set to FALSE some settings will be calculated, e.g. if the
#' cache folder is set to FALSE the full path will be calculated using the
#' main folder, or if the verbosity is not set the default verbosity will be
#' returned. If raw is set to TRUE settings are returned as they are currently
#' stored.
#' @param verbose boolean deciding whether status information/updates should be shown or not
#' @return A config list with all settings currently set for the madrat package
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}, \code{\link{initializeConfig}}
#' @export
getConfig <- function(option=NULL, raw=FALSE, verbose=TRUE) {
  initializeConfig(verbose=verbose)
  
  cfg <- getOption("madrat_cfg")
  
  n <- c("sourcefolder"="sources", "cachefolder"="cache/default", "mappingfolder"="mappings", "outputfolder"="output/default")
  for(p in c("sourcefolder", "cachefolder", "mappingfolder", "outputfolder")){
    if(is.na(cfg[[p]]) & !raw) cfg[[p]] <- paste0(cfg$mainfolder,"/",n[p])
  }

  if(is.null(option)) return(cfg)
  return(cfg[[option]])
}


