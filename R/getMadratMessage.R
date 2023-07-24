#' Tool: getMadratMessage
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
    deps <- suppressWarnings(getDependencies(sub("^.*:::", "", fname),
                                             packages = getConfig("packages"), self = TRUE)$func)
    for (n in names(x)) {
      x[[n]] <- x[[n]][sub("^.*:::", "", names(x[[n]])) %in% deps]
    }
  }
  if (!is.null(name)) x <- x[[name]]
  return(x)
}
