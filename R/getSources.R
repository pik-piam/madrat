#' getSources
#' 
#' These functions can be used to retrieve a list of currently available
#' sources and outputs (based on the availability of corresponding conversion
#' functions in the loaded data data processing packages.)
#' 
#' 
#' @aliases getSources
#' @param type Type of source, either set to "global" "regional", "download", "correct" or
#' NULL. Global returns all global sources (non-regional), regional returns
#' sources with regional data, "download" returns source for which a download
#' function is available, "correct" returns sources for which a correct function
#' is available and NULL returns all available sources
#' @param packages A character vector with packages for which the available Sources/Calculations should be returned
#' @param globalenv Boolean deciding whether sources/calculations in the global environment should be included or not
#' @return A vector containing all currently available sources or outputs of
#' all loaded data processing packages.
#' @note Please be aware that these functions assume that required source files
#' do exist and are set correctly in the corresponding config file.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}, \code{\link{setConfig}}
#' @examples
#' 
#' print(getSources())
#' 
#' @export getSources
getSources <- function(type=NULL, packages=getConfig("packages"), globalenv=getConfig("globalenv")) {
  n <- NULL
  for(p in packages) {
    n <- c(n,ls(getNamespace(p)))
  }
  if(globalenv) {
    n <- c(n,ls(as.environment(".GlobalEnv")))
  }
  .filter <- function(x,pattern) {
    tmp <- sub(pattern,"",grep(pattern,n,value=TRUE))
    return(tmp[tmp!="Source"])
  }
  read     <- .filter(n,"read")
  correct  <- .filter(n,"correct")
  convert  <- .filter(n,"convert")
  download <- .filter(n,"download")
  
  if(is.null(type)) {
    out <- read    
  } else if(type=="global") {
    out <- setdiff(read,convert)
  } else if(type=="regional") {
    out <- intersect(read,convert)
  } else if(type=="correct") {
    out <- correct
  } else if(type=="download") {
    out <- download
  } else {
    stop("Unknown type")
  }
  return(out)
}
