#' Tool: cacheArgumentsHash
#'
#' Calculate hash from given function arguments for given call
#'
#' @param functionName A function name as a string (usually in the form package:::fun, e.g. madrat:::calcTauTotal).
#' Passing a vector of functions is possible, but is only intended
#' for corresponding read/correct/convert functions. If multiple functions in a vector define arguments with the same
#' name but different default values only the default defined in the first function is considered.
#' @param args A list of named arguments used to call the given function(s). If duplicates of arguments exists the first
#' occurrence of the argument will be used.
#' @param errorOnMismatch Whether an error is thrown in case an argument in args is not accepted by functionName.
#' @return A hash representing the given arguments hash for the given call. NULL, if no argument deviates from the
#' default argument settings.
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}, \code{\link{getNonDefaultArguments}}
#' @examples
#' madrat:::cacheArgumentsHash("madrat:::readTau", args = list(subtype = "historical"))
#' madrat:::cacheArgumentsHash("madrat:::readTau", args = list(subtype = "paper"))
#' functionNames <- c("madrat:::readTau", "madrat:::convertTau")
#' madrat:::cacheArgumentsHash(functionNames, args = list(subtype = "historical"))
cacheArgumentsHash <- function(functionName, args = NULL, errorOnMismatch = TRUE) {
  setWrapperInactive("wrapperChecks")

  isTerraObject <- function(x) {
    return("terra" %in% attr(class(x), "package"))
  }
  if (any(vapply(args, isTerraObject, logical(1)))) {
    warning("avoid terra object (e.g. SpatRaster) arguments to madrat functions, ",
            "they cause unpredictable cache behavior")
  }

  nonDefaultArguments <- getNonDefaultArguments(functionName, args, errorOnMismatch)

  if (length(nonDefaultArguments) == 0) {
    return(NULL)
  }

  nonDefaultArguments <- nonDefaultArguments[robustOrder(names(nonDefaultArguments))]
  return(paste0("-", digest::digest(nonDefaultArguments, algo = getConfig("hash"))))
}
