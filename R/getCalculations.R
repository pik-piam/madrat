#' getCalculations
#' 
#' This function can be used to retrieve a list of currently available
#' sources and outputs (based on the availability of corresponding conversion
#' functions in the loaded data data processing packages.)
#' 
#' 
#' @aliases getCalculations
#' @param prefix Type of calculations. Available options are "download" (source download), 
#' "read" (source read), "correct" (source corrections), "convert" (source conversion to ISO countries),
#' "calc" (further calculations), and "full" (collections of calculations)
#' @param packages A character vector with packages for which the available Sources/Calculations should be returned
#' @param globalenv Boolean deciding whether sources/calculations in the global environment should be included or not
#' @return A data frame containing all currently available outputs of
#' all loaded data processing packages including its name, its function call and its package origin.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}, \code{\link{setConfig}}
#' @examples
#' 
#' print(getCalculations())
#' print(getCalculations("read"))
#' 
#' @export
#' 
getCalculations <- function(prefix="calc", packages=getConfig("packages"), globalenv=getConfig("globalenv")) {
  x <- NULL
  for(p in packages) {
    tmp <- data.frame(type=ls(getNamespace(p)),package=p)
    x <- rbind(x,tmp)
  }
  if(globalenv) {
    tmp <- data.frame(type=ls(as.environment(".GlobalEnv")),package=".GlobalEnv")
    x <- rbind(x,tmp)
  }
  .filter <- function(x,pattern) {
    x <- x[grep(pattern,x$type),]
    x$type <- sub(pattern,"",x$type)
    return(x)
  }
  out <- .filter(x,paste0("^",prefix))
  if(dim(out)[1]==0) return(NULL)
  out$call <- paste0(out$package,":::",prefix,out$type)
  out$call <- sub(".GlobalEnv:::","",out$call, fixed=TRUE)
  return(out[!(out$type %in% c("Source","Output")),])
}