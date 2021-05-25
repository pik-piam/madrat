#' getNonDefaultArguments
#'
#' Given a function and an argument list, identify which arguments are different from their default.
#'
#' @param call A function name as a string or symbol. Passing a vector of functions is possible, but is only intended
#' for corresponding read/correct/convert functions. If multiple functions in a vector define arguments with the same
#' name but different default values only the default defined in the first function is considered.
#' @param args A list of named arguments used to call the given function(s). If duplicates of arguments exists the first
#' occurrence of the argument will be used.
#' @return A subset of args that is used by the function/s and is different from default values.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cacheArgumentsHash}}, \code{\link{toolstartmessage}}
#' @examples
#' madrat:::getNonDefaultArguments("madrat:::readTau", args = list(subtype = "historical"))
#' madrat:::getNonDefaultArguments("madrat:::readTau", args = list(subtype = "paper"))
#' calls <- c(madrat:::readTau, madrat:::convertTau)
#' madrat:::getNonDefaultArguments(calls, args = list(subtype = "historical"))
getNonDefaultArguments <- function(call, args = NULL) {
  if (length(args) == 0) {
    return(NULL)
  }
  if (length(call) == 0) {
    stop("No call provided for argument hash calculation!")
  }

  .tmp <- function(call) {
    if (is.character(call)) call <- eval(parse(text = call))
    return(formals(call))
  }
  if (length(call) > 1) {
    defargs <- unlist(lapply(call, .tmp))
    defargs <- defargs[!duplicated(names(defargs))]
  } else {
    defargs <- .tmp(call)
  }

  commonargs <- intersect(names(defargs), names(args))
  if (!("..." %in% names(defargs))) args <- args[commonargs]

  for (i in commonargs) {
    if (identical(defargs[[i]], args[[i]])) args <- args[names(args) != i]
  }
  if (length(args) == 0) {
    return(NULL)
  }
  return(args)
}
