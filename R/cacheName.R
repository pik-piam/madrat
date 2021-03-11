#' Tool: cacheGet
#' 
#' Load fitting cache data (if available)
#' 
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}. 
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param mustExist Boolean which decides whether the cache file must exist or not.
#' In case of FALSE the potential file name is returned. When set to TRUE, a file
#' name will only be returned if the file exists (otherwise NULL) and in combination
#' which \code{setConfig(forcecache=TRUE)} even a cache file with deviating hash
#' might get selected.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @return cached data, if cache is available, otherwise NULL
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' madrat:::cacheName("calc","TauTotal", packages="madrat")
#' @importFrom digest digest

cacheName <- function(prefix, type, args=NULL,  graph=NULL, mustExist = FALSE, ...) {
  fpprefix <- prefix
  if (fpprefix %in% c("convert", "correct")) fpprefix <- "read"
  fp <- fingerprint(name = paste0(fpprefix, type), graph = graph, ...)
  if (length(args) == 0) args <- NULL
  if (!is.null(args)) args <- paste0("-",digest(args[order(names(args))], algo = getConfig("hash")))
  .isSet <- function(prefix, type, setting) {
    return(all(getConfig(setting) == TRUE) || any(c(type, paste0(prefix,type)) %in% getConfig(setting)))
  }
  .fname <- function(prefix,type,fp,args) {
    return(paste0(getConfig("cachefolder"),"/",prefix,type,"-",fp,args,".rds"))
  }
  fname <- .fname(prefix,type,fp,args)
  if (file.exists(fname) || !mustExist) return(fname)
  if (!.isSet(prefix,type,"forcecache")) {
    vcat(2, paste0(" - Cache file ",basename(fname)," does not exist"), show_prefix = FALSE)
    return(NULL)
  }
  # no perfectly fitting file exists, try to find a similar one
  files <- Sys.glob(.fname(prefix,type,"*",args))
  if (length(files) == 0) {
    vcat(2, " - No fitting cache file available", show_prefix = FALSE)
    return(NULL)
  }
  if (length(files) == 1) file <- files
  else file <- files[order(file.mtime(files), decreasing = TRUE)][1]
  vcat(1," - force cache", basename(file),"with unmatching fingerprint (", fp,")", 
       fill = 300, show_prefix = FALSE)
  return(file)
}



