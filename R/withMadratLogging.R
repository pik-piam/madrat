#' Tool: withMadratLogging
#'
#' Function will activate madrat logging facilities for all code provided
#' to this function. This means that \code{message}, \code{warning} and
#' \code{stop} calls will also report to the madrat log output
#'
#'
#' @param expr expression to be evaluated.
#' @param logOnly passed to vcat, determines if warning/error is thrown after logging
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{vcat}}
#' @examples
#' \dontrun{
#' madrat:::withMadratLogging(message("Hello world!"))
#' }
#'
withMadratLogging <- function(expr, logOnly = TRUE) {
  if (isWrapperActive("callingHandler")) {
    return(expr)
  }

  messageHandler <- function(w) {
    w$message <- sub("\n$", "", w$message)
    vcat(1, w$message)
    if (!isWrapperActive("vcat")) invokeRestart("muffleMessage")
  }

  warningHandler <- function(w) {
    vcat(0, w$message, logOnly = logOnly)
  }

  errorHandler <- function(w) {
    vcat(-1, w$message, logOnly = logOnly)
  }

  setWrapperActive("callingHandler")
  withCallingHandlers(expr,
    message = messageHandler,
    warning = warningHandler,
    error   = errorHandler
  )
}
