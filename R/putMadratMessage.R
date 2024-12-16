#' putMadratMessage
#'
#' Store a madrat message in the madrat environment. The madrat environment
#' behaves similar like global options, except that 1) messages will also
#' be stored in cache files and restored when a cache file is being loaded and
#' 2) messages are always stored in lists with messages split by function calls
#' where the message was triggered.
#'
#' @param name The category in which the message should be stored
#' @param value The message that should be recorded as character. Alternatively,
#' if \code{name} is not set, it is also possible to provide a complete list
#' of the structure value[[name]][[fname]] where name and fname correspond
#' to the category name and function name entries (e.g.
#' \code{value = list(test = list(readTau = "This is a toast"))}).
#' @param fname function name the entry belongs to or the frame number from which
#' the function name should be derived from (e.g. -1 to recieve function name
#' from parent function).
#' @param add boolean deciding whether the value should be added to a existing
#' value (TRUE) or overwrite it (FALSE)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{putMadratMessage}}
#' @examples
#' putMadratMessage("test", "This is a toast", fname = "readTau")
#' getMadratMessage("test", fname = "calcTauTotal")
#' @export

putMadratMessage <- function(name, value, fname = -1, add = FALSE) {
  if (missing(name)) name <- NULL
  if (is.null(name) && is.list(value) && !is.null(names(value))) {
    for (n in names(value)) {
      for (f in names(value[[n]])) {
        putMadratMessage(name = n, value = value[[n]][[f]], fname = f, add = add)
      }
    }
  } else {
    if (is.numeric(fname)) fname <- as.character(sys.call(fname))[1]
    madratMessage <- getOption("madratMessage")
    if (is.null(name)) name <- names(madratMessage)
    for (n in name) {
      madratMessage[[name]][[fname]] <- if (add) c(madratMessage[[name]][[fname]], value) else value
    }
    options(madratMessage = madratMessage) # nolint: undesirable_function_linter.
  }
}
