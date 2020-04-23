#' getDependencies
#' 
#' Returns information about dependencies of a 
#' madrat-based calc- read- or full-function.
#' 
#' 
#' @param name name of the function to be analyzed
#' @param direction Character string, either “in”, “out” or “all”. If “in” 
#' all sources feeding into the function are listed. If “out” consumer of the 
#' function are listed. If “all” returns the union of these.
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
  tmp <- igraph::subcomponent(igraph::graph_from_data_frame(graph),name,direction)
  tmp <- attr(tmp,"names")
  return(setdiff(tmp,name))
}

