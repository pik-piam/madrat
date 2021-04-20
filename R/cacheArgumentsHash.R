#' Tool: cacheArgumentsHash
#' 
#' Calculate hash from given function arguments for given call
#' 
#' @param call function call
#' @param args a list of named arguments used to call the given function
#' @return hash representing the given arguments hash for the given call. 
#' NULL, if no argument deviates from the default argument settings
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' madrat:::cacheName("calc","TauTotal")
#' @importFrom digest digest

cacheArgumentsHash <- function(call, args=NULL) {
  if (length(args) == 0) return(NULL)
  
  if (is.character(call)) call <- eval(parse(text = call))
  defargs <- formals(call)
  
  commonargs <- intersect(names(defargs), names(args))
  if (!("..." %in% names(defargs))) args <- args[commonargs]
  
  for (i in commonargs) {
    if (is.null(defargs[[i]]) || is.null(args[[i]])) {
      if (is.null(defargs[[i]]) && is.null(args[[i]])) args <- args[names(args) != i]
      next
    } 
    if (defargs[[i]] == args[[i]]) args <- args[names(args) != i]
  }
  if (length(args) == 0) return(NULL)
  if (!is.null(args)) args <- paste0("-",digest(args[order(names(args))], algo = getConfig("hash")))
  return(args)
}


