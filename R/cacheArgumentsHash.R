#' Tool: cacheArgumentsHash
#'
#' Calculate hash from given function arguments for given call
#'
#' @param call A function as a string or symbol. Passing a vector of functions is possible, but is only intended for
#' corresponding read/correct/convert functions. If multiple functions in a vector define arguments with the same name
#' but different default values only the default defined in the first function is considered.
#' @param args A list of named arguments used to call the given function(s). If duplicates of arguments exists the first
#' occurrence of the argument will be used.
#' @return A hash representing the given arguments hash for the given call. NULL, if no argument deviates from the
#' default argument settings.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}, \code{\link{getNonDefaultArguments}}
#' @examples
#' madrat:::cacheArgumentsHash("madrat:::readTau", args = list(subtype = "historical"))
#' madrat:::cacheArgumentsHash("madrat:::readTau", args = list(subtype = "paper"))
#' calls <- c(madrat:::readTau, madrat:::convertTau)
#' madrat:::cacheArgumentsHash(calls, args = list(subtype = "historical"))
#' @importFrom digest digest
cacheArgumentsHash <- function(call, args = NULL) {
  setWrapperInactive("wrapperChecks")

  nonDefaultArguments <- getNonDefaultArguments(call, args)
  nonDefaultArguments <- nonDefaultArguments[robustOrder(names(nonDefaultArguments))]

  if (length(nonDefaultArguments) == 0) {
    return(NULL)
  }
  return(paste0("-", digest(nonDefaultArguments, algo = getConfig("hash"))))
}
