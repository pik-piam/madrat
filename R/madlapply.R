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
  exptoeval <- paste0("clusterEvalQ(cl=cl, expr=library(", expr, "))")

 return(eval(parse(text=as.expression(exptoeval))))
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
#' If your are using madlapply insice a calcFunction you don't have to export or evaluate any objects and functions or packages. 
#' }
#' @return A list with one entry for each element in X
#' @author Stephen Wirth, Jan Philipp Dietrich
#' @importFrom parallel makeCluster clusterExport clusterEvalQ stopCluster parLapply
#' @export
#'
#' @examples
#' 
#'library(madrat)
#'library(magclass)
#'
#'input <- #input
#'array(2, c(10,5,1))
#'powwrap <- function(#input=array(2, c(10,5,1))
#'){
#'# create a variable which has to be exported to the workers
#'pow <- function(exponent){
#'  # as magclass is not part of base, therefore the library
#'  #(magclass has to be evaluated on all the workers)
#'  return(as.magpie(
#'  input^exponent))
#'  }
#'  
#'  #actuall madlapply call 
#'  resultnopar <- madlapply(X=c(2:10), FUN=pow, #stating X and FUN
#'                exports=list(list(c("input"),expression(environment()))),
#'                 # listing the objects or function 
#'                 #to be exported and their origin environments
#'               evals=c("magclass" )) # libraries to be evaluated 
#'                                                
#'                                                
#'                 return(resultnopar)
#'  }
#'  
#'  
#'  res <- powwrap()
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
 
    cl <- makeCluster(getConfig("nocores"))
    on.exit(stopCluster(cl))
    envirFUN = environment(FUN)
 
    #invisible(
    sapply(X = exports, FUN = clusterExportHelper, cl=cl, envirFUN=envirFUN )#)
   
  #invisible(
 sapply(X=evals, FUN = clusterEvalQHelper, cl=cl)#)
    return(parLapply(cl=cl, X=X, fun=FUN, ... ))
    }
}