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
#' functions history will be returned.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMadratMessage}}
#' @examples
#' madrat:::putMadratMessage("This is a test", "test")
#' madrat:::getMadratMessage("test")

getMadratMessage <- function(name = NULL, fname = NULL) {
  x <- getOption("madratMessage")
  if(!is.null(x) && !is.null(fname)) {
    deps <- suppressWarnings(getDependencies(sub("^.*:::", "", fname),
                                             packages = getConfig("packages"), self = TRUE)$call)
    for(n in names(x)) {
      x[[n]] <- x[[n]][names(x[[n]]) %in% deps]
    }
  }
  if(!is.null(name)) x <- x[[name]]
  return(x)
}
