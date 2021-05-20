#' Tool: Start message
#'
#' Function writes a process start message and performs some diagnostics
#'
#'
#' @param level This argument allows to establish a hierarchy of print
#' statements. The hierarchy is preserved for the next vcat executions.
#' Currently this setting can have 4 states: NULL (nothing will be changed), 0
#' (reset hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
#' hierarchy level by 1).
#' @param argumentValues list of the evaluated arguments of the calling function
#' @return a list containing diagnostic information required by \code{\link{toolendmessage}}
#' @author Jan Philipp Dietrich, Pascal Führlich
#' @seealso \code{\link{toolendmessage}}, \code{\link{vcat}}
#' @examples
#'
#' innerFunction <- function() {
#'   startinfo <- madrat:::toolstartmessage(list(argumentsToPrint = 123), "+")
#'   vcat(1, "inner")
#'   madrat:::toolendmessage(startinfo, "-")
#' }
#' outerFunction <- function() {
#'   startinfo <- madrat:::toolstartmessage(list(), "+")
#'   vcat(1, "outer")
#'   innerFunction()
#'   madrat:::toolendmessage(startinfo, "-")
#' }
#' outerFunction()

toolstartmessage <- function(argumentValues, level = NULL) {
  functionAndArgs <- as.list(sys.call(-1))
  theFunction <- functionAndArgs[[1]]
  nonDefaultArguments <- getNonDefaultArguments(eval(theFunction), argumentValues)

  argsString <- paste0(list(nonDefaultArguments))  # wrap everything in list for nicer string output
  argsString <- substr(argsString, 6, nchar(argsString) - 1)  # remove superfluous list from string

  if (nchar(argsString) <= getConfig("maxLengthLogMessage")) {
    functionCallString <- paste0(theFunction, "(", argsString, ")", collapse = "")
    hint <- ""
  } else {
    functionCallString <- paste0(deparse(sys.call(-1)), collapse = "")
    hint <- paste0(" -- to print evaluated arguments: setConfig(maxLengthLogMessage = ", nchar(argsString), ")")
  }

  vcat(1, "Run ", functionCallString, hint, level = level, fill = 300, show_prefix = FALSE)
  return(list(time1 = proc.time(), functionCallString = functionCallString))
}
