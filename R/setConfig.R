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
#' @param enablecache Is deprecated and will be ignored. Please use
#' \code{ignorecache} instead.
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
#' @param pucfolder The path where portable unaggregated collection (puc) files
#' are located. NA by default, which means <mainfolder>/puc
#' @param tmpfolder Path to a temp folder for temporary storage of files. By default
#' set to <mainfolder>/tmp
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
#' @param diagnostics Either FALSE (default) to avoid the creation of additional
#' log files or a file name for additional diagnostics information (without file ending).
#' @param debug Boolean which activates a debug mode. In debug mode all calculations will
#' be executed with try=TRUE so that calculations do not stop even if the previous calculation failed.
#' This can be helpful to get a full picture of errors rather than only seeing the first one. In addition
#' debug=TRUE will add the suffix "debug" to the files created to avoid there use in productive runs.
#' Furthermore, with debug=TRUE calculations will be rerun even if a corresponding tgz file
#' already exists.
#' @param maxLengthLogMessage in log messages evaluated arguments are printed if the resulting message
#' is shorter than this value, otherwise arguments are shown as passed, potentially with unevaluated variable names
#' @param redirections A list of source folder redirections, intended to be set
#' by \code{\link{redirectSource}}. See that function's documentation for more details.
#' @param .cfgchecks boolean deciding whether the given inputs to setConfig should be checked for
#' consistency or just be accepted (latter is only necessary in very rare cases and should not be used
#' in regular cases)
#' @param .verbose boolean deciding whether status information/updates should be shown or not
#' @param .local boolean deciding whether options are only changed until the end of the current function execution
#' OR environment for which the options should get changed.
#' @note \code{setConfig} must only be used before the data processing is started and changes in the configuration
#' from within a download-, read-, correct-, convert-, calc-, or full-function are not allowed! Only allowed
#' configuration update is to add another \code{extramapping} via \code{\link{addMapping}}.
#' Currently the use of \code{setConfig} within any of these functions will trigger a warning, which is planned
#' to be converted into an error message in one of the next package updates!
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getConfig}}, \code{\link{getISOlist}}
#' @examples
#' \dontrun{
#' setConfig(forcecache = c("readSSPall", "convertSSPall"))
#' }
#' @importFrom utils installed.packages
#' @importFrom withr local_options
#' @export
setConfig <- function(regionmapping = NULL, # nolint
                      extramappings = NULL,
                      packages = NULL,
                      globalenv = NULL,
                      enablecache = NULL,
                      verbosity = NULL,
                      mainfolder = NULL,
                      sourcefolder = NULL,
                      cachefolder = NULL,
                      mappingfolder = NULL,
                      outputfolder = NULL,
                      pucfolder = NULL,
                      tmpfolder = NULL,
                      nolabels = NULL,
                      forcecache = NULL,
                      ignorecache = NULL,
                      cachecompression = NULL,
                      hash = NULL,
                      diagnostics = NULL,
                      debug = NULL,
                      maxLengthLogMessage = NULL,
                      redirections = NULL,
                      .cfgchecks = TRUE,
                      .verbose = TRUE,
                      .local = FALSE) {

  if (isWrapperActive("wrapperChecks")) {
    for (w in c("downloadSource", "readSource", "calcOutput", "retrieveData")) {
      if (isWrapperActive(w)) {
        warning("setConfig must not be used from within ", w, "!")
        break
      }
    }
  }
  setWrapperInactive("wrapperChecks")

  cfg <- getConfig(raw = TRUE, verbose = .verbose)

  if (is.environment(.local)) {
    .localEnvir <- .local
    .local <- TRUE
  } else {
    .localEnvir <- parent.frame()
  }

  firstsetting <- TRUE
  info <- NULL

  if (!is.null(regionmapping) && file.exists(regionmapping)) {
    regionmapping <- normalizePath(regionmapping)
  }

  if (!is.null(enablecache)) {
    warning('Argument "enablecache" is deprecated and will be ignored, use "ignorecache" instead!')
    enablecache <- NULL
  }

  if (!is.null(packages)) {
    packages <- unique(packages, fromLast = TRUE)
    if (.cfgchecks) {
      missing <- setdiff(packages, rownames(installed.packages()))
      if (length(missing) > 0) {
        stop("Setting \"packages\" can only be set to installed packages (missing: \"",
             paste(missing, collapse = "\", \""), "\")")
      }
    }
  }

  args <- names(formals(setConfig))
  args <- grep("^\\.", args, value = TRUE, invert = TRUE)

  for (x in args) {
    if (!is.null(get(x))) {
      value <- get(x)
      if (x == "extramappings" && length(value) == 1 && value == "") value <- NULL
      # additional checks/modifications if input is a folder
      if (grepl("folder", x, fixed = TRUE)) {
        if (!is.na(value)) {
          if (x == "cachefolder" && !grepl("[\\\\/]", value)) {
            value <- file.path(cfg$mainfolder, "cache", value)
          }
          # normalize path value
          if (!file.exists(value)) {
            dir.create(value, recursive = TRUE)
            if (.verbose) {
              vcat(-2, paste("created folder", sub("/$", "", normalizePath(value, winslash = "/")), "..."), fill = 300)
            }
          }
          value <-  sub("/$", "", normalizePath(value, winslash = "/"))
        }
      }
      if (firstsetting) {
        if (.local) {
          info <- "Local configuration update:"
        } else {
          info <- "Global configuration update:"
        }
      }
      firstsetting <- FALSE
      info <- c(info, paste0("  ", x, ": ", paste(cfg[[x]], collapse = ", "), " -> ", paste(value, collapse = ", ")))
      cfg[[x]] <- value
    }
  }
  if (.local) {
    # change options until the function calling this function exits
    local_options(madrat_cfg = cfg, .local_envir = .localEnvir)
  } else {
    options(madrat_cfg = cfg) # nolint
  }

  if (!is.null(info) && .verbose) {
    for (i in info) vcat(-2, i)
  }
}

#' @describeIn setConfig A wrapper for setConfig(..., .local = TRUE)
#' @param ... Arguments forwarded to setConfig
#' @export
# setting .local = TRUE would only set config until localConfig ends
localConfig <- function(...) setConfig(..., .local = parent.frame())
