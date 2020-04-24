#' getDependencies
#' 
#' Returns information about dependencies of a 
#' madrat-based calc- read- or full-function.
#' 
#' 
#' @param name name of the function to be analyzed
#' @param direction Character string, either “in”, “out”, "both", “full”, "din" or
#' "dout". If “in” all sources feeding into the function are listed. If “out” 
#' consumer of the function are listed. If “both” the union of "in" and "out" is returned.
#' If "full" the full network this function is connected to is shown, including
#' indirect connections to functions which neither source nor consume the given
#' function but serve as sources to other consumer functions. "din" and "dout" (short
#' for "direct in" and "direct out") behave like "in" and "out" but only show direct
#' calls in or from the function (ignoring the network of functions attached to it).
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}. 
#' Will be created with \code{\link{getMadratGraph}} if not provided. 
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCalculations}}, \code{\link{getMadratGraph}},  \code{\link{getMadratInfo}}
#' @export

getDependencies <- function(name, direction="in", graph=NULL, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) stop("Package \"igraph\" needed for this function to work.")
  if(is.null(graph)) graph <- getMadratGraph(...)
  if(!(name %in% c(graph$from,graph$to))) stop("There is no function with the name \"",name,"\"")
  ggraph <- igraph::graph_from_data_frame(graph)
  if(direction=="full") direction <- "all"
  if(direction=="both") {
    tmp <- unique(c(attr(igraph::subcomponent(ggraph,name,"in"),"names"),
                    attr(igraph::subcomponent(ggraph,name,"out"),"names")))
  } else if(direction=="dout") {
    tmp <- sort(unique(graph$to[graph$from==name]))
  } else if(direction=="din") {
    tmp <- sort(unique(graph$from[graph$to==name]))
  } else {
    tmp <- attr(igraph::subcomponent(ggraph,name,direction),"names")
  }
  tmp <- setdiff(tmp,name)
  
  packages <- c(graph$from_package,graph$to_package)
  names(packages) <- c(graph$from,graph$to)
  
  out <- data.frame(func=tmp,package=packages[tmp],row.names=NULL)
  
  return(data.frame(out[order(out$package),],row.names=NULL))
}

