#' Tool: cacheArgumentsHash
#' 
#' Calculate hash from given function arguments for given call
#' 
#' @param call function call or vector of functions calls
#' @param args a list of named arguments used to call the given function(s). If duplicates
#' of arguments exists the first occurrence of the argument will be used.
#' @return hash representing the given arguments hash for the given call. 
#' NULL, if no argument deviates from the default argument settings
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' madrat:::cacheArgumentsHash("madrat:::readTau", args=list(subtype="historical"))
#' madrat:::cacheArgumentsHash("madrat:::readTau", args=list(subtype="paper"))
#' calls <- c(madrat:::readTau, madrat:::convertTau)
#' madrat:::cacheArgumentsHash(calls, args=list(subtype="historical"))
#' @importFrom digest digest

getNonDefaultArguments <- function(call, args=NULL) {
  if (length(args) == 0) return(NULL)
  
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
  if (length(args) == 0) return(NULL)
  if (!is.null(args)) args <- paste0("-",digest(args[order(names(args), method = "radix")], algo = getConfig("hash")))
  return(args)
}
