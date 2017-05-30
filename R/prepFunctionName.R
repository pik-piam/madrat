#' prepFunctionName
#' 
#' Function to prepare a function call for a given type and prefix
#' 
#' 
#' @aliases prepFunctionName
#' @param type name of calculation/source
#' @param prefix Type of calculations. Available options are "download" (source download), 
#' "read" (source read), "correct" (source corrections), "convert" (source conversion to ISO countries),
#' "calc" (further calculations), and "full" (collections of calculations)
#' @param years which should be provided to the function (if any)
#' @param error_on_missing boolean deciding whether a missing type should throw an error or return NULL
#' @return A function call as character to the specified function with corresponding package as attribute
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}, \code{\link{setConfig}}
#' @examples
#' 
#' print(madrat:::prepFunctionName("Tau","read"))
#' print(madrat:::prepFunctionName("TauTotal","calc"))
#' print(madrat:::prepFunctionName("EXAMPLE","full"))
#' @importFrom utils tail

prepFunctionName <- function(type, prefix="calc", years=NULL, error_on_missing=TRUE) {
  getCalc <- getCalculations(prefix = prefix)
  if(!(type %in% getCalc$type)) {
    if(error_on_missing) {
      stop("Type \"",type, "\" is not a valid output type. Check getCalculations(\"",prefix,"\") for available types!")
    } else {
      return(NULL)
    }
  }
  # creates functionname and checks that all arguments
  # expected by the function are actually beeing provided
  name <- getCalc$call[getCalc$type==type]
  package <- getCalc$package[getCalc$type==type]
  if(length(name)>1) {
    name <- as.character(tail(name,1))
    package <- as.character(tail(package,1))
    if(package==".GlobalEnv" & type %in% c("TauTotal","Tau")) {
      stop("Cannot substitute package internal function for type \"",type,"\" and prefix \"",prefix,"\" with function in global environment. Please use other function name instead!")  
    } else {
      warning("More than one function found for type \"",type,"\" and prefix \"",prefix,"\". Use last occurrence (package \"",package,"\")")
    }
  }
  
  # extract arguments which agree between the given function and its corresponding wrapper function
  # (because they have to be mentioned specifically in the function call)
  
  wrapper <- list(calc     = "calcOutput", 
                  download = "downloadSource",
                  read     = "readSource", 
                  correct  = "readSource",
                  convert  = "readSource",
                  full     = "retrieveData")
  
  fformals <- names(formals(eval(parse(text = name))))
  wformals <- names(formals(as.character(wrapper[prefix])))

  if("..." %in% wformals) {
    formals <- intersect(wformals,fformals)
    fcomplete <- all(fformals %in% formals)
  } else {
    formals <- fformals
    fcomplete <- TRUE
  }
  formals <- setdiff(formals,"...")
  if(is.null(years)) formals <- setdiff(formals,"years")
  args <- paste(formals,formals,sep="=")
  if("..." %in% wformals & !fcomplete) args <- c(args,"...")
  out <- paste0(name,'(',paste(args,collapse=", "),')')
  
  attr(out,"package") <- package
  if(package==".GlobalEnv") {
    attr(out,"pkgcomment") <- ".GlobalEnv"
  } else {
    attr(out,"pkgcomment") <- paste(package,packageDescription(package)$Version)
  }
  return(out)
}