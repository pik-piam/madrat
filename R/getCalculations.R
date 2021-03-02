#' getCalculations
#' 
#' This function can be used to retrieve a list of currently available
#' sources and outputs (based on the availability of corresponding conversion
#' functions in the loaded data data processing packages.)
#' 
#' 
#' @aliases getCalculations
#' @param prefix Type of calculations, vector of types or search term (e.g. "read|calc"). Available options are "download" (source download), 
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
  if (length(prefix) > 1) prefix <- paste(prefix, collapse = "|")
  x <- NULL
  for (p in packages) {
    tmp <- try(data.frame(type = ls(getNamespace(p)), package = p), silent = TRUE)
    if (class(tmp) != "tryError") x <- rbind(x,tmp)
  }
  if (globalenv) {
    tmp <- ls(as.environment(".GlobalEnv"))
    if (length(tmp) > 0) {
      tmp <- data.frame(type = tmp, package = ".GlobalEnv")
      x <- rbind(x,tmp)
    }
  }

  pattern <- paste0("^",prefix)
  x <- x[grep(pattern,x$type),]
  if (dim(x)[1] == 0) return(NULL)
  x$call <- paste0(x$package,":::",x$type)
  x$type <- sub(pattern,"",x$type)  
  x$call <- sub(".GlobalEnv:::","",x$call, fixed = TRUE)
  return(x[!(x$type %in% c("Source","Output")),])
}