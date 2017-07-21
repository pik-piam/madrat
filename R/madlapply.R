clusterExportHelper <- function(export, cl, envirFUN ){
  if(!is.list(export)){ stop("Argument provided in export is not a list")}
  if(length(export)!=2){stop("Argument export contains ", length(export), " elements, instead of 2!")}
  if(!is.vector(export[[1]])) {stop("First element of export is not a character vector")}
  if(!is.expression(export[[2]]) & !is.character(export[[2]])){stop("Second element of export is neither a character nor an expression!")}
  if(is.expression(export[[2]])){ 
    return(clusterExport(cl=cl, varlist= export[[1]], envir=eval(export[[2]], envir = envirFUN)))
  }
  else{ return(clusterExport(cl=cl, varlist= export[[1]], envir=export[[2]]))
  }
}

clusterEvalQHelper <- function(expr, cl){
  exptoeval <- paste0("clusterEvalQ(cl=cl, expr=library(", expr, "))")#, character.only=TRUE))")
  #return(as.expression(exptoeval))
  return(eval(as.expression(exptoeval) ))
  #return(clusterEvalQ(cl=cl, expr=library(expr, character.only = TRUE)))
}


#' @title madlapply
#'
#' @description { Wrapper Function that executes multiple function calls on one core using 
#' lapply or on the supplied number of cores using parLapply if the parallel option is set to TRUE.
#' Make sure the package \code{\link[parallel]{parallel}} is installed and runs on your machine before 
#' setting parallel to TRUE.
#' }
#' @param X a vector (atomic or list) or an \code{\link{expression}} object. Other objects
#'  (including classed objects) will be coerced by base::\code{\link{as.list}}
#' @param FUN the function to be applied to each element of X
#' @param exports A list containing a list for each environment which contains imports. Each list contains a character vector and a character or an expression.
#' The first listing all Objects and Functions to be Exported. The second lists the associated environment. The environment in which the expression is evaluated is
#' the environment of FUN.
#' @param evals List of expressions to evaluate. See details for more information
#' @param ... additional arguments to pass to FUN: beware of partial matching to earlier arguments.
#'
#' @details {
#' The function madlapply can currently only be called inside calcFunctions as shown in the example.
#' Since madrat is designed to call Functions using the wrapper-function calcOutput this is usually not an issue.
#' If you want to parallelise a functoin call from the global environment or another function not called by \code{\link{calcOutput}} you have to set up your cluster manually (see also \code{\link[parallel]{parallel}}).
#' We are currently working on that issue and will release a fixed version soon.
#' }
#' @return A list with one entry for each element in X
#' @author Stephen Wirth, Jan Philipp Dietrich
#' @importFrom parallel makeCluster clusterExport clusterEvalQ stopCluster parLapply
#' @export
#'
#' @examples
#' 
#'  # To view the source code of a 
#'  text <- madrat:::calcPow
#'  text
#'  
#'  \dontrun{
#'  #Since devtools::check() throws an error running this code I had to add the dontrun closure,
#'  but it actually can be run. 
#'  #Nevetheless running this code won't give you much insight in how madlapply works.
#' setConfig(enablecache=FALSE)
#' calcOutput("Pow", aggregate=FALSE)
#' setConfig(parallel=TRUE)
#' calcOutput("PoW", aggregate=FALSE)
#' }
#' 
#' 
#' 
#
#' @export
madlapply <- function(X=NULL, FUN=NULL, exports=NULL, evals=NULL, ...){
  #@TODO: maybe parse function code for variable names left and right hand of an assignment 
  #and check which left hand variable aren't created inside the function, then import those.
  #Problem: How to know the environment they were created.
  #@TODO: evaluate namespace of moinput package rather then evaluate whole libraries?
  
  if(!getConfig("parallel")){
    return(lapply(X = X, FUN = FUN, ...))
  } 
  else{
   # if(any(!is.expression(evals))){stop("At least one elemnt of evals is not an expression!")}
  
    cl <- makeCluster(getConfig("nocores"))
    on.exit(stopCluster(cl))
    envirFUN = environment(FUN)
    invisible(sapply(X = exports, FUN = clusterExportHelper, cl=cl, envirFUN=envirFUN ))
    #exprs <- lapply(X=evals, FUN = clusterEvalQHelper, cl=cl)
    #do.call(eval, exprs)
   
  invisible(sapply(X=evals, FUN = clusterEvalQHelper, cl=cl))
    return(parLapply(cl=cl, X=X, fun=FUN, ... ))
    }
}