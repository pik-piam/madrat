#' Tool: fingerprint
#' 
#' Function which creates a unique fingerprint for a madrat function based on
#' the code of the function itself, of all other madrat functions which feed
#' this function and of all source folders involved in the process.
#' The fingerprint can serve as an indication whether the workflow for the given
#' function has been most likely changed, or not. If all involved source folders
#' and the code of all involved functions remains the same, also the fingerprint
#' will stay the same, otherwise it will change. Hence, it can be used as an
#' indication whether calculations needs to be redone or not.
#' It is used to figure out whether a cache file can be used for further
#' calculations, or whether the calculation should be redone.
#' 
#' @note For a better performance not the files in a folder itself are hashed
#' but the last modified dates of these files.
#' As the fingerprint function only takes madrat-based functions into account
#' (e.g. read-functions or calc-functions), but does ignore all other functions
#' there might be instances in which the workflow actually would lead to 
#' other numbers but the fingerprint stays the same. In a similar fashion it is
#' possible that the fingerprint changes even so the workflow stayed the same 
#' (as the dependencies are sometimes overestimated).
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

fingerprint <- function(name, details=FALSE, graph = NULL, ...) {
  d <- getDependencies(name, direction = "in", self=TRUE, graph = graph, ...)
  
  dr <- d[d$type=="read",]
  if(dim(dr)[1]>0) {
    sources <- substring(dr$func,5)
    prefix <- c("download", "read", "convert", "correct")
    read_functions <- paste0(rep(dr$package, each=length(prefix)),":::",paste0(prefix, sources))
  } else {
    sources <- NULL
    read_functions <- NULL
  }
  do <- d[d$type!="read",]
  if(dim(do)[1]>0) {
    other_functions <- paste0(do$package,":::",do$func)
  } else {
    other_functions <- NULL
  }
  funcs <- sort(sub("^\\.GlobalEnv:::","",c(read_functions, other_functions)))
  
  fpfu <- fingerprintFunction(sort(funcs))
  if(length(sources)>0) {
    fpfo <- fingerprintFolder(paste0(getConfig("sourcefolder"),"/",sort(sources)))
  } else {
    fpfo <- NULL
  }
  
  fp <- c(fpfu, fpfo)
  out <- digest(unname(fp), algo = "xxhash32")
  if(details) attr(out,"details") <- fp
  return(out)
}

fingerprintFunction <- function(name) {
  .tmp <- function(x) {
    f <- try(eval(parse(text=x)), silent = TRUE)
    if("try-error" %in% class(f)) return(NULL)
    return(digest(deparse(f), algo = "xxhash32"))
  }
  return(unlist(sapply(name, .tmp)))
}

fingerprintFolder <- function(folder) {
  .tmp <- function(f) return(digest(file.mtime(sort(list.files(f,recursive=TRUE, full.names=TRUE))), algo = "xxhash32"))
  return(sapply(folder, .tmp))
}
