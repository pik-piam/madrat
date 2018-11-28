#' setConfig
#' 
#' This function manipulates the current madrat configuration. 
#' In general, NULL means that the argument remains as it is whereas
#' all other inputs will overwrite the current setting.
#' 
#' 
#' @param regionmapping The name of the csv file containing the region mapping
#' that should be used for aggregation (e.g. "regionmappingREMIND.csv").
#' @param packages A character vector with packages in which corresponding 
#' read and calc functions should be searched for
#' @param globalenv Boolean deciding whether sources/calculations in the global 
#' environment should be included or not
#' @param enablecache Boolean deciding whether data should be read from cache
#' if data is available and the up-to-date (data will always be written to the
#' cache regardless of this setting)
#' @param verbosity an integer value describing the verbosity of the functions
#' (2 = full information, 1 = only warnings and execution information, 0 = only
#' warnings, -1 = no information)
#' @param mainfolder The mainfolder where all data can be found and should be
#' written to.
#' @param sourcefolder The folder in which all source data is stored (in
#' sub-folders with the name of the source as folder name). In the default case
#' this argument is set to NA meaning that the default folder should be used
#' which is <mainfolder>/sources
#' @param cachefolder The folder in which all cache files should be written to.
#' In the default case this argument is set to NA meaning that the default
#' folder should be used which is <mainfolder>/cache
#' @param mappingfolder A folder containing all kinds of mappings (spatial,
#' temporal or sectoral). In the default case this argument is set to NA
#' meaning that the default folder should be used which is
#' <mainfolder>/mappings
#' @param outputfolder The folder all outputs should be written to. In the
#' default case this argument is set to NA meaning that the default folder
#' should be used which is <mainfolder>/output
#' @param pop_threshold Population threshold in capita which determines whether
#' the country is put into the "important" or "dispensable" class in
#' \code{\link{getISOlist}}. This distinction is used for different treatment
#' of countries in notifications to set a focus on rather critical issues
#' instead of flooding the user with information.
#' @param forcecache Argument that allows to force madrat to read data from
#' cache if the corresponding cache files exist. It is either a boolean to
#' fully activate or deactivate the forcing or a vector of files (e.g. readTau, calcTauTotal) 
#' or type (e.g. Tau, TauTotal) that should be read from cache in any case.
#' @param delete_cache Boolean deciding whether a temporary cache folder (as
#' created by retrieveInput) should be deleted after completion or not.
#' @param diagnostics file name for additional diagnostics information (without file ending).
#' 3 diagnostic files will be written if a file name is provided (a csv showing the network 
#' of function executions, a log file showing the log and a full log showing the full amount
#' of available information.)
#' @param .cfgchecks boolean deciding whether the given inputs to setConfig should be checked for
#' consistency or just be accepted (latter is only necessary in very rare cases and should not be used
#' in regular cases)
#' @param .verbose boolean deciding whether status information/updates should be shown or not
#' @param  ignorecache Argument that allows madrat to ignore the forcecache argument for the
#'  given vector of files (e.g. readTau, calcTauTotal) or types 
#'  (e.g. Tau, TauTotal) called by calcOutput or readSource.
#'  The top level function must always be part of this list.
#' @param nocores  integer number of cores to use for \code{\link[parallel]{clusterApply}} calls
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getConfig}}, \code{\link{getISOlist}}
#' @examples
#' 
#'  \dontrun{
#'    setConfig(forcecache=c("readSSPall","convertSSPall"))  
#'  }
#' @importFrom utils installed.packages
#' @export
setConfig <- function(regionmapping=NULL, 
                      packages=NULL,
                      globalenv=NULL,
                      enablecache=NULL, 
                      verbosity=NULL,
                      mainfolder=NULL,
                      sourcefolder=NULL,
                      cachefolder=NULL,
                      mappingfolder=NULL,
                      outputfolder=NULL,
                      pop_threshold=NULL,
                      forcecache=NULL,
                      ignorecache = NULL,
                      delete_cache=NULL,
                      diagnostics=NULL,
                      nocores=NULL,
                      .cfgchecks=TRUE,
                      .verbose=TRUE){
  cfg <- getConfig(raw=TRUE, verbose=.verbose)

  firstsetting <- TRUE
  info <- NULL
  
  if(!is.null(packages)) {
    packages <- unique(packages,fromLast=TRUE)
    if(.cfgchecks) {
      missing <- setdiff(packages,rownames(installed.packages()))
      if(length(missing) > 0) stop("Setting \"packages\" can only be set to installed packages (missing: \"",paste(missing,collapse="\", \""),"\")")
    }
  }
  
  args <- names(formals(setConfig))
  args <- grep("^\\.",args,value=TRUE,invert=TRUE)
  
  for(x in args) {
    if(!is.null(get(x))) {
      value <- get(x)
      #additional checks/modifications if input is a folder
      if(grepl("folder",x,fixed = TRUE)) {
        if(!is.na(value)) {
          #normalize path value
          if(!file.exists(value)) {
            dir.create(value,recursive = TRUE)
            if(.verbose) vcat(-2,paste("created folder",sub("/$","",normalizePath(value,winslash = "/")),"..."), fill=300)
          }
          value <-  sub("/$","",normalizePath(value,winslash = "/"))
        }
      }
      if(firstsetting)   info <- "Configuration update:"
      firstsetting <- FALSE
      info <- c(info,paste0("  ",x,": ",paste(cfg[[x]],collapse=", ")," -> ",paste(value,collapse=", ")))
      cfg[[x]] <- value
    }
  }
  options(madrat_cfg = cfg)
  if(!is.null(info) & .verbose) {
    for(i in info) vcat(-2,i)
  }
}
