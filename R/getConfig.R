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
#' @param wrappercheck boolean (de)activating a check whether the function is called from within one of the
#' madrat wrapper (\code{readSource}, \code{calcOutput}, ...) or not. Do not use this setting except you know
#' exactly what you are doing! Should be kept TRUE in nearly all cases!
#' @return A config list with all settings currently set for the madrat package
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}, \code{\link{initializeConfig}}
#' @export
getConfig <- function(option = NULL, raw = FALSE, verbose = TRUE, print = FALSE, wrappercheck = TRUE) {
  initializeConfig(verbose = verbose)

  if (wrappercheck && isWrapperActive("wrapperChecks")) {
    for (w in c("downloadSource", "readSource", "calcOutput")) {
      if (isWrapperActive(w)) {
        warning("getConfig for option \"", option, "\" must not be used from within ", w, "!")
        break
      }
    }
  }

  cfg <- getOption("madrat_cfg")
  cfg$mainfolder <- sub("/$", "", cfg$mainfolder)

  n <- c("sourcefolder" = "sources", "cachefolder" = "cache/default",
         "mappingfolder" = "mappings", "outputfolder" = "output")
  for (p in c("sourcefolder", "cachefolder", "mappingfolder", "outputfolder")) {
    if (is.na(cfg[[p]]) & !raw) cfg[[p]] <- paste0(cfg$mainfolder, "/", n[p])
  }
  if (verbose && print) {
    nmax <- max(nchar(names(cfg)))
    vcat(1, "", show_prefix = FALSE)
    vcat(1, "Current madrat configuration:", show_prefix = FALSE)
    for (n in names(cfg)) {
      quotes <- ifelse(is.character(cfg[[n]]), "\"", "")
      value <- cfg[[n]]
      if (is.null(value)) value <- "NULL"
      vcat(1, paste0("   ", format(n, width = nmax), " -> ",
                     paste0(quotes, value, quotes, collapse = ", ")), show_prefix = FALSE)
    }
    vcat(1, "", show_prefix = FALSE)
  }

  if (is.null(option)) return(cfg)
  return(cfg[[option]])
}
