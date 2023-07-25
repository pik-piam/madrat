#' getMadratMessage
#'
#' Read a madrat message from the madrat environment. The madrat environment
#' behaves similar like global options, except that 1) messages will also
#' be stored in cache files and restored when a cache file is being loaded and
#' 2) messages are always stored in lists with messages split by function calls
#' where the message was triggered.
#'
#' @param name The category in which the message should be stored
#' @param fname function name. If specified only messages belonging to the
#' functions history will be returned (this includes entries from the function
#' itself, but also entries from functions which were called by this function).
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMadratMessage}}
#' @examples
#' putMadratMessage("test", "This is a toast", fname = "readTau")
#' getMadratMessage("test", fname = "calcTauTotal")
#' @export

getMadratMessage <- function(name = NULL, fname = NULL) {
  x <- getOption("madratMessage")
  if (!is.null(x) && !is.null(fname)) {
    fname <- sub("^.*:::", "", fname)
    fnameBase <- sub("^(download|correct|convert)", "read", fname)
    deps <- try(suppressWarnings(getDependencies(fnameBase,
                                                 packages = getConfig("packages"), self = TRUE)$func))
    if (inherits(deps, "try-error")) return(NULL)
    readDeps <- sub("^read", "", grep("^read", deps, value = TRUE))
    if (length(readDeps) > 0) {
      convertDeps <- paste0("convert", readDeps)
      correctDeps <- paste0("correct", readDeps)
      downloadDeps <- paste0("download", readDeps)
      deps <- c(deps, convertDeps, correctDeps, downloadDeps)
    }
    for (n in names(x)) {
      x[[n]] <- x[[n]][sub("^.*:::", "", names(x[[n]])) %in% deps]
    }
  }
  if (!is.null(name)) x <- x[[name]]
  return(x)
}
