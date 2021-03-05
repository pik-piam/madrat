#' Tool: cacheGet
#' 
#' Load fitting cache data (if available)
#' 
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}. 
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @return cached data, if cache is available, otherwise NULL
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' madrat:::cacheGet("calc","TauTotal", packages="madrat")
#' @importFrom digest digest
cacheGet <- function(prefix, type, args=NULL, graph = NULL, ...) {
  .isSet <- function(prefix, type, setting) {
    return(all(getConfig(setting) == TRUE) || any(c(type, paste0(prefix,type)) %in% getConfig(setting)))
  }
  
  if (.isSet(prefix,type,"ignorecache") || !getConfig("enablecache")) return(NULL)
  
  fname <- cacheName(prefix = prefix, type = type, args = args,  graph = graph, mustExist = TRUE, ...) 
  
  if (is.null(fname)) return(NULL)
  
  vcat(2," - loading cache", basename(fname), fill = 300, show_prefix = FALSE)
  x <- try(readRDS(fname), silent = TRUE)
  if ("try-error" %in% class(x)) {
    vcat(-2, " - corrupt cache file ", basename(fname),"! Continue without cache.")
    return(NULL)
  }
  attr(x,"id") <- fname
  return(x)
}