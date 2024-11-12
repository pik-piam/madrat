#' Tool: Start message
#'
#' This function writes a process start message (what function was called with which arguments) and stores the current
#' time, so the corresponding call to \code{\link{toolendmessage}} can calculate the elapsed time.
#'
#' @param functionName The name of the calling function as a string.
#' @param argumentValues A list of the evaluated arguments of the calling function.
#' @param level This argument allows to establish a hierarchy of print statements. The hierarchy is preserved for the
#' next vcat executions. Currently this setting can have 4 states: NULL (nothing will be changed), 0 (reset
#' hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease hierarchy level by 1).
#' @return A list containing diagnostic information required by \code{\link{toolendmessage}}.
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{toolendmessage}}, \code{\link{vcat}}
#' @importFrom utils str
#' @examples
#'
#' innerFunction <- function() {
#'   startinfo <- madrat:::toolstartmessage("innerFunction", list(argumentsToPrint = 123), "+")
#'   vcat(1, "inner")
#'   madrat:::toolendmessage(startinfo, "-")
#' }
#' outerFunction <- function() {
#'   startinfo <- madrat:::toolstartmessage("outerFunction", list(), "+")
#'   vcat(1, "outer")
#'   innerFunction()
#'   madrat:::toolendmessage(startinfo, "-")
#' }
#' outerFunction()
toolstartmessage <- function(functionName, argumentValues, level = NULL) {
  setWrapperInactive("wrapperChecks")

  functionCallString <- functionCallString(functionName, argumentValues)

  vcat(1, "Run ", functionCallString, level = level, fill = 300, show_prefix = FALSE)
  return(list(time1 = proc.time(), functionCallString = functionCallString))
}

functionCallString <- function(functionName, argumentValues) {
  nonDefaultArguments <- getNonDefaultArguments(functionName, argumentValues)
  argsString <- paste0(list(nonDefaultArguments)) # wrap everything in list for nicer string output
  argsString <- substr(argsString, 6, nchar(argsString) - 1) # remove superfluous list from string

  callWithEvaluatedArgs <- paste0(functionName, "(", argsString, ")")
  if (nchar(callWithEvaluatedArgs) <= getConfig("maxLengthLogMessage")) {
    functionCallString <- callWithEvaluatedArgs
  } else {
    functionCallString <- paste0(deparse(sys.call(-1)), collapse = "")
    if (nchar(functionCallString) > getConfig("maxLengthLogMessage")) {
      functionCallString <- paste0(substr(callWithEvaluatedArgs, 1,
                                          getConfig("maxLengthLogMessage") - 3), "...")
    }
  }
  return(functionCallString)
}
