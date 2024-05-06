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
#' @param print if TRUE and verbose is TRUE a configuration overview will also get printed
#' @note \code{getConfig} is primarily designed to make the overall madrat
#' configuration available to system tools of the madrat framework. There are
#' only a few exceptions for which configuration settings are also readable
#' from within a download-, read-, convert-, correct-, calc- or full-function.
#' These exceptions are the setting "debug" (which can be used to add additional
#' debug messages when active), the "tmpfolder" which can be used to temporarily
#' store data and the setting "hash" (which can only be accessed from within a
#' full function and can there be used to apply the identical hash algorithm for
#' other calculations in which hashing is being used).
#' Besides that "regionmapping" and "extramappings" can also be read from within
#' calc- and full-functions but their use is at least for the calc-functions
#' discouraged as it either might lead to incorrect caching behavior, or - if
#' implemented correctly - lead to significant slow-downs of overall calculations.
#' All other settings are currently still accessible but trigger a warning that
#' this option will soon be removed. So, please make sure that your code runs
#' without reading these options!
#' As a background note: Read access to these settings will be restricted as they
#' otherwise would allow access to code elements or data in a form which is
#' violating the overall madrat logic and thereby can lead to erroneous results.
#' @return A config list with all settings currently set for the madrat package
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}, \code{\link{initializeConfig}}
#' @export
getConfig <- function(option = NULL, raw = FALSE, verbose = TRUE, print = FALSE) { # nolint
  initializeConfig(verbose = verbose)

  allowedOptions <- list(downloadSource = c("debug", "tmpfolder"),
                         readSource = c("debug", "tmpfolder"),
                         calcOutput = c("regionmapping", "extramappings", "debug", "tmpfolder"),
                         retrieveData = c("regionmapping", "extramappings", "debug", "hash", "tmpfolder"))

  discouragedOptions <- list(calcOutput = c("regionmapping", "extramappings"))

  if (isWrapperActive("wrapperChecks")) {
    for (w in c("downloadSource", "readSource", "calcOutput", "retrieveData")) {
      if (isWrapperActive(w)) {
        if (is.null(option)) {
          warning("getConfig must not be used from within ", w, "! Access will be disabled soon!")
          break
        } else if (!(option %in% allowedOptions[[w]])) {
          warning("getConfig for option \"", option, "\" must not be used from within ", w,
                  "! Access will be disabled soon!")
          break
        } else if (option %in% discouragedOptions[[w]]) {
          message("getConfig for option \"", option, "\" should - if possible - be avoided from within ", w, "!")
          break
        }
      }
    }
  }

  cfg <- getOption("madrat_cfg")
  cfg$mainfolder <- sub("/$", "", cfg$mainfolder)

  if (!raw) {
    folders <- c(sourcefolder = "sources", cachefolder = "cache/default", mappingfolder = "mappings",
                 outputfolder = "output", pucfolder = "puc", tmpfolder = "tmp")
    for (folderName in names(folders)) {
      if (is.null(cfg[[folderName]]) || is.na(cfg[[folderName]])) {
        cfg[[folderName]] <- file.path(cfg$mainfolder, folders[folderName])
      }
      dir.create(cfg[[folderName]], showWarnings = FALSE, recursive = TRUE)
    }

    if (is.null(cfg[["redirections"]])) {
      cfg[["redirections"]] <- list()
    }
  }
  if (verbose && print) {
    nmax <- max(nchar(names(cfg)))
    vcat(1, "", show_prefix = FALSE)
    vcat(1, "Current madrat configuration:", show_prefix = FALSE)
    for (configName in names(cfg)) {
      quotes <- ifelse(is.character(cfg[[configName]]), "\"", "")
      value <- cfg[[configName]]
      if (is.null(value)) value <- "NULL"
      vcat(1, paste0("   ", format(configName, width = nmax), " -> ",
                     paste0(quotes, value, quotes, collapse = ", ")), show_prefix = FALSE)
    }
    vcat(1, "", show_prefix = FALSE)
  }

  if (is.null(option)) return(cfg)
  return(cfg[[option]])
}
