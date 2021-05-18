#' getNonDefaultArguments
#' 
#' Given a function and an argument list, identify which arguments are different from their default.
#' 
#' @param aFunction a single function
#' @param args a list of named arguments used to call the given function. If duplicates of arguments exists the first
#' occurrence of the argument will be used.
#' @return a named list of the arguments that are actually used and have a non-default value
#' @author Jan Philipp Dietrich, Pascal Führlich
#' @examples
#' madrat:::getNonDefaultArguments(madrat:::readTau, args = list(subtype = "historical"))
#' madrat:::getNonDefaultArguments(madrat:::readTau, args = list(subtype = "paper"))
getNonDefaultArguments <- function(aFunction, args = list()) {
  defaultArgs <- formals(eval(aFunction))

  commonArgs <- intersect(names(defaultArgs), names(args))
  if (!("..." %in% names(defaultArgs))) {
    args <- args[commonArgs]
  }

  for (i in commonArgs) {
    if (identical(defaultArgs[[i]], args[[i]])) {
      args <- args[names(args) != i]
    }
  }

  return(args[robustOrder(names(args))])
}
