#' getSources
#' 
#' These functions can be used to retrieve a list of currently available
#' sources and outputs (based on the availability of corresponding conversion
#' functions in the loaded data data processing packages.)
#' 
#' 
#' @aliases getSources
#' @param name name of function for which sources should get returned. If not specified, all sources in the
#' specified environment are returned
#' @param type Type of source, either set to "read", "convert", "correct", "download" or
#' NULL. If specified, a vector containing the sources with the corresponding function type are returned,
#' otherwise a data.frame with all sources and their available function types is returned.
#' @param packages A character vector with packages for which the available Sources/Calculations should be returned
#' @param globalenv Boolean deciding whether sources/calculations in the global environment should be included or not
#' @return A vector or data.frame containing all corresponding sources
#' @note Please be aware that these functions only check the availability of corresponding functions of the package, not
#' whether the functions will properly work.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}, \code{\link{setConfig}}
#' @examples
#' 
#' print(getSources())
#' 
#' @export getSources
getSources <- function(name=NULL, type=NULL, packages=getConfig("packages"), globalenv=getConfig("globalenv")) {
  n <- NULL
  for(p in packages) {
    n <- c(n,ls(getNamespace(p)))
  }
  if(globalenv) {
    n <- c(n,ls(as.environment(".GlobalEnv")))
  }
  .filter <- function(pattern,x) {
    tmp <- sub(pattern,"",grep(pattern,n,value=TRUE))
    return(tmp[tmp!="Source"])
  }
  
  types <- c("read","correct","convert","download")
  
  if(!is.null(name)) {
    filter <- substring(getDependencies(name, type="read", packages=packages, globalenv=globalenv)$func,5)
    pattern <- paste0("^(",paste(types,collapse="|"),")")
    n <- n[sub(pattern,"",n) %in% filter]
  }
  
  if(is.null(type)) {
    tmp <- sapply(types,.filter,n)
    out <- data.frame(source=unique(unlist(tmp)), stringsAsFactors = FALSE)
    for(i in names(tmp)) out[i] <- (out$source %in% tmp[[i]])
    return(out)
  } else if(type %in% types) {
    return(.filter(type,n))
  } else {
    stop("Unknown type")
  }
}
