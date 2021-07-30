#' setConfig
#' 
#' This function manipulates the current madrat configuration. 
#' In general, NULL means that the argument remains as it is whereas
#' all other inputs will overwrite the current setting.
#' For values which can be reset to NULL (currently only "extramappings")
#' you can achieve a reset by setting the value to "".
#' 
#' 
#' @param regionmapping The name of the csv file containing the region mapping
#' that should be used for aggregation (e.g. "regionmappingREMIND.csv").
#' @param extramappings Names of additional mappings supplementing the given
#' region mapping. This allows for additional aggregation levels such as
#' subnational aggregation.
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
#' @param nolabels vector of retrieve models (e.g. "EXAMPLE" in case of "fullEXAMPLE") 
#' which should NOT apply a replacement of known hashes with given code labels
#' @param forcecache Argument that allows to force madrat to read data from
#' cache if the corresponding cache files exist. It is either a boolean to
#' fully activate or deactivate the forcing or a vector of files (e.g. readTau, calcTauTotal) 
#' or type (e.g. Tau, TauTotal) that should be read from cache in any case.
#' @param  ignorecache Argument that allows madrat to ignore the forcecache argument for the
#'  given vector of files (e.g. readTau, calcTauTotal) or types 
#'  (e.g. Tau, TauTotal) called by calcOutput or readSource.
#'  The top level function must always be part of this list.
#' @param cachecompression logical or character string specifying whether cache files
#' use compression. TRUE corresponds to gzip compression, and character strings "gzip", 
#' "bzip2" or "xz" specify the type of compression.
#' @param hash specifies the used hashing algorithm. Default is "xxhash32" and
#' all algorithms supported by \code{\link[digest]{digest}} can be used.
#' @param delete_cache Boolean deciding whether a temporary cache folder (as
#' created by retrieveInput) should be deleted after completion or not.
#' @param diagnostics file name for additional diagnostics information (without file ending).
#' 2 log files are written if a file name is provided (a compact version with the most
#' relevant information and a full version with all available details).
#' @param nocores integer number of cores to use
#' @param debug Boolean which activates a debug mode. In debug mode all calculations will
#' be executed with try=TRUE so that calculations do not stop even if the previous calculation failed.
#' This can be helpful to get a full picture of errors rather than only seeing the first one. In addition
#' debug=TRUE will add the suffix "debug" to the files created to avoid there use in productive runs.
#' Furthermore, with debug=TRUE calculations will be rerun even if a corresponding tgz file 
#' already exists.
#' @param indentationCharacter character used for indenting the output of nested function calls
#' @param maxLengthLogMessage in log messages evaluated arguments are printed if the resulting message
#' is shorter than this value, otherwise arguments are shown as passed, potentially  with unevaluated variable names
#' @param .cfgchecks boolean deciding whether the given inputs to setConfig should be checked for
#' consistency or just be accepted (latter is only necessary in very rare cases and should not be used
#' in regular cases)
#' @param .verbose boolean deciding whether status information/updates should be shown or not
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
                      extramappings=NULL,
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
                      nolabels=NULL,
                      forcecache=NULL,
                      ignorecache = NULL,
                      cachecompression=NULL,
                      hash=NULL,
                      delete_cache=NULL,
                      diagnostics=NULL,
                      nocores=NULL,
                      debug=NULL,
                      indentationCharacter=NULL,
                      maxLengthLogMessage=NULL,
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
      if(x == "extramappings" && value == "") value <- NULL
      #additional checks/modifications if input is a folder
      if(grepl("folder",x,fixed = TRUE)) {
        if(!is.na(value)) {
          if(x=="cachefolder" && !grepl("[\\\\/]",value)) {
            value <- file.path(cfg$mainfolder,"cache",value)
          }
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
