#' resetMadratMessages
#'
#' Delete stored madrat messages from the madrat environment. The madrat environment
#' behaves similar like global options, except that 1) messages will also
#' be stored in cache files and restored when a cache file is being loaded and
#' 2) messages are always stored in lists with messages split by function calls
#' where the message was triggered.
#'
#' @param name The category for which the messages should be reset (if not set
#' messages in all categories will be reset)
#' @param fname function name for which the entries should be reset (if not specified
#' messages for all function names will be reset)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{putMadratMessage}}, \code{\link{getMadratMessage}}
#' @examples
#' putMadratMessage("test", "This is a toast", fname = "readTau")
#' getMadratMessage("test", fname = "calcTauTotal")
#' resetMadratMessages("test")
#' @export

resetMadratMessages <- function(name = NULL, fname = NULL) {

  madratMessage <- getOption("madratMessage")

  if (is.null(name) && is.null(fname)) {
    madratMessage <- NULL
  } else if (is.null(fname)) {
    madratMessage[[name]] <- NULL
  } else if (is.null(name)) {
    for (n in names(madratMessage)) {
      madratMessage[[n]][[fname]] <- NULL
    }
  } else {
    madratMessage[[name]][[fname]] <- NULL
  }
  if (length(madratMessage) == 0) madratMessage <- NULL
  options(madratMessage = madratMessage) # nolint: undesirable_function_linter.
}
