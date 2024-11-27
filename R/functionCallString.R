#' functionCallString
#'
#' Create a string representation for a function call. If the resulting string
#' is longer than getConfig("maxLengthLogMessage") arguments are printed as
#' passed (e.g. as variable name instead of the evaluated content of that
#' variable), if that is still too long it is cropped.
#'
#' @param functionName name of the called function
#' @param argumentValues the list of arguments passed
#' @return A string representing the given function call
#'
#' @author Pascal Sauer
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
