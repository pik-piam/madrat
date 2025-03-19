#' getNonDefaultArguments
#'
#' Given a function and an argument list, identify which arguments are different from their default.
#'
#' @param functionName A function name as a string (usually in the form package:::fun, e.g. madrat:::calcTauTotal).
#' Passing a vector of functions is possible, but is only intended
#' for corresponding read/correct/convert functions. If multiple functions in a vector define arguments with the same
#' name but different default values only the default defined in the first function is considered.
#' @param args A list of named arguments used to call the given function(s). If duplicates of arguments exists the first
#' occurrence of the argument will be used.
#' @param errorOnMismatch Whether an error is thrown in case an argument in args is not accepted by functionName.
#' @return A subset of args that is used by the function/s and is different from default values.
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{cacheArgumentsHash}}, \code{\link{toolstartmessage}}
#' @examples
#' madrat:::getNonDefaultArguments("madrat:::readTau", args = list(subtype = "historical"))
#' madrat:::getNonDefaultArguments("madrat:::readTau", args = list(subtype = "paper"))
#' functionNames <- c(madrat:::readTau, madrat:::convertTau)
#' madrat:::getNonDefaultArguments(functionNames, args = list(subtype = "historical"))
getNonDefaultArguments <- function(functionName, args, errorOnMismatch = TRUE) {
  if (length(args) == 0) {
    return(NULL)
  } else if (length(functionName) == 0) {
    stop("No functionName provided for argument hash calculation!")
  }

  .getFormals <- function(functionName) {
    if (is.character(functionName)) {
      functionName <- eval(parse(text = functionName))
    }
    return(formals(functionName))
  }
  if (length(functionName) > 1) {
    defargs <- do.call(c, lapply(functionName, .getFormals))
    defargs <- defargs[!duplicated(names(defargs))]
  } else {
    defargs <- .getFormals(functionName)
  }

  commonargs <- intersect(names(defargs), names(args))
  unmatchedArgs <- args[!(args %in% args[commonargs])]
  if (errorOnMismatch && !("..." %in% names(defargs)) && length(unmatchedArgs) >= 1) {
    if (length(names(defargs)) == 0) {
      acceptedArgs <- " does not support any arguments)"
    } else {
      acceptedArgs <- paste0(" only accepts the following arguments: ",
                             paste(names(defargs), collapse = ", "), ")")
    }
    stop("The following unexpected arguments were passed to ",
         paste(functionName, collapse = " / "), ": ",
         paste(names(unmatchedArgs), collapse = ", "),
         "\n(", paste(functionName, collapse = " / "),
         acceptedArgs)
  }

  for (i in commonargs) {
    if (identical(defargs[[i]], args[[i]])) {
      args <- args[names(args) != i]
    }
  }
  if (length(args) == 0) {
    return(NULL)
  }
  return(args)
}
