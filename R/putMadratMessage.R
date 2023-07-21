#' Tool: putMadratMessage
#'
#' Store a madrat message in the madrat environment. The madrat environment
#' behaves similar like global options, except that 1) messages will also
#' be stored in cache files and restored when a cache file is being loaded and
#' 2) messages are always stored in lists with messages split by function calls
#' where the message was triggered.
#'
#' @param value The message that should be recorded
#' @param name The category in which the message should be stored
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{putMadratMessage}}
#' @examples
#' madrat:::putMadratMessage("This is a test", "test")

putMadratMessage <- function(value, name = NULL) {
  if(!is.null(name)) {
    tmp <- list()
    tmp[[name]] <- value
    value <- tmp
  }
  madratMessage <- getOption("madratMessage")
  for (n1 in names(value)) {
    if(!is.list(madratMessage[[n1]])) madratMessage[[n1]] <- list()
    for(n2 in names(value[[n1]])) {
      madratMessage[[n1]][[n2]] <- value[[n1]][[n2]]
    }
  }
  options(madratMessage = madratMessage) # nolint: undesirable_function_linter
}
