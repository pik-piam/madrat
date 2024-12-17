#' wrappedReadSourceFunctions
#'
#' Minimal wrapper for readSource.
#' Beware that no checks are done that the input follows the required naming conventions.
#'
#' @param read A function to read the data. Mandatory. Function name must start with 'read'. The
#' rest of the function name determines the 'type'.
#' @param correct A function to correct the data. Optional. Function name must start with 'correct'
#' and end with the type, same as for read.
#' @param covert A function to convert the data (putting it at country level). Optional. Function
#' name must start with 'convert' and end with the type, same as for read.
#' @param download A function to download the data. Optional. Currently not used.
#' @inheritParams readSource
#' @inherit readSource return
#'
#' @author Sally Dacie
#' @export
wrappedReadSourceFunctions <- function(read, correct = NULL, convert = NULL, download = NULL,
                                       subtype = NULL, subset = NULL) {

  # get string/bool from function(s) for use in readSource
  .getConvert <- function(convert, correct) {
    if (!is.null(convert)) {
      convertArg <- TRUE
    } else if (!is.null(correct)) {
      convertArg <- "onlycorrect"
    } else {
      convertArg <- FALSE
    }
    return(convertArg)
  }

  # get default subtype argument if available, otherwise NULL
  if (is.null(subtype)) {
    subtype <- formals(eval(read))[["subtype"]]
  }

  envList <- as.list(environment(read))
  envList <- envList[startsWith(names(envList), "read")]
  functionName <- names(envList)[Position(function(f) identical(f, read), envList)]
  type <- sub("^read", "", functionName)

  convertArg <- .getConvert(convert = convert, correct = correct)

  # call readSource with original interface
  readSource(type = type, subtype = subtype, subset = subset, convert = convertArg)
}
