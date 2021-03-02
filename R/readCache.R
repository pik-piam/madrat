#' Tool: fromCache
#' 
#' Read data from fitting cache file, if available and activated, 
#' otherwise return NULL
#' 
#' @param name Name of the function to be analyzed
#' @param details Boolean indicating whether additional details in form
#' of an attribute with underlying hash information should be added or not
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}. 
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @return A md5-based fingerprint of all provided sources
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}
#' @examples
#' madrat:::fingerprint("toolGetMapping")
#' @importFrom digest digest


.isSet <- function(prefix, type, setting) {
  return(all(getConfig(setting) == TRUE) || any(c(type, paste0(prefix,type)) %in% getConfig(setting)))
}

nameCache <- function(prefix, type, args=NULL,  graph=NULL, ...) {
  fp <- fingerprint(name = paste0(prefix, type), graph = graph, ...)
  if (!is.null(args)) args <- paste0("-",digest(args[order(names(args))], algo = "xxhash32"))
  .fname <- function(prefix,type,fp,args) {
    return(paste0(getConfig("cachefolder"),"/",prefix,type,"-",fp,args,".rds"))
  }
  fname <- .fname(prefix,type,fp,args)
  if (.isSet(prefix,type,"forcecache")) {
    # if fitting cache file exists, return that one
    if(file.exists(fname)) return(fname)
    # no perfectly fitting file exists, try to find a similar one
    files <- Sys.glob(.fname(prefix,type,"*",args))
    if(length(files)==0) return(fname)
    if(length(files)==1) file <- files
    else file <- files[order(file.mtime(files), decreasing = TRUE)][1]
    vcat(1," - force cache", basename(file),"with unmatching fingerprint (", fp,")", 
         fill=300, show_prefix=FALSE)
    return(file)
  }
  return(fname)
}

fromCache <- function(prefix, type, args=NULL, graph = NULL, ...) {
  
  if (.isSet(prefix,type,"ignorecache") || !getConfig("enablecache")) return(NULL)
  
  fname <- nameCache(prefix = prefix, type = type, args = args,  graph = graph, ...) 
  
  if (!file.exists(fname)) {
    vcat(2, paste0(" - Cache file ",basename(fname)," does not exist"), show_prefix = FALSE)
    return(NULL)
  }
  
  vcat(2," - loading cache", basename(fname), fill=300, show_prefix=FALSE)
  x <- try(readRDS(fname), silent = TRUE)
  if("try-error" %in% class(x)) {
    vcat(-2, " - corrupt cache file ", basename(fname),"! Continue without cache.")
    return(NULL)
  }
  attr(x,"id") <- fname
  return(x)
}
