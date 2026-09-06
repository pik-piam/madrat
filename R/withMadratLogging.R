#' Tool: withMadratLogging
#'
#' Function will activate madrat logging facilities for all code provided
#' to this function. This means that \code{message}, \code{warning} and
#' \code{stop} calls will also report to the madrat log output
#'
#'
#' @param expr expression to be evaluated.
#' @param logOnly passed to vcat, determines if warning/error is thrown after logging.
#'   If omitted in a nested call, the enclosing logging policy is inherited.
#' @param warningsAsErrors whether warnings should be logged and handled as errors.
#'   If omitted in a nested call, the enclosing logging policy is inherited.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{vcat}}
#' @keywords internal
#' @examples
#' \dontrun{
#' madrat:::withMadratLogging(message("Hello world!"))
#' }
#'
withMadratLogging <- function(expr, logOnly = TRUE, warningsAsErrors = FALSE) {
  loggingPolicy <- getOption(
    "madrat_loggingPolicy",
    list(logOnly = TRUE, warningsAsErrors = FALSE)
  )
  if (!missing(logOnly)) loggingPolicy$logOnly <- logOnly
  if (!missing(warningsAsErrors)) loggingPolicy$warningsAsErrors <- warningsAsErrors
  withr::local_options(madrat_loggingPolicy = loggingPolicy)

  if (isWrapperActive("callingHandler")) {
    return(expr)
  }

  messageHandler <- function(w) {
    w$message <- sub("\n$", "", w$message)
    vcat(1, w$message)
    if (!isWrapperActive("vcat")) invokeRestart("muffleMessage")
  }

  warningHandler <- function(w) {
    loggingPolicy <- getOption("madrat_loggingPolicy")
    verbosity <- ifelse(loggingPolicy$warningsAsErrors, -1, 0)
    vcat(verbosity, w$message, logOnly = loggingPolicy$logOnly)
  }

  errorHandler <- function(w) {
    loggingPolicy <- getOption("madrat_loggingPolicy")
    vcat(-1, w$message, logOnly = loggingPolicy$logOnly)
  }

  setWrapperActive("callingHandler")
  withCallingHandlers(expr,
    message = messageHandler,
    warning = warningHandler,
    error   = errorHandler
  )
}
