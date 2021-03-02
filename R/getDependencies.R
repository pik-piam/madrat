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
#' @param type type filter. Only dependencies of that type will be returned. Currently available
#' types are "calc", "read" and "tool"
#' @param self boolean defining whether the function itself, which is analyzed, should be 
#' included in the output, or not
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCalculations}}, \code{\link{getMadratGraph}},  \code{\link{getMadratInfo}}
#' @importFrom igraph graph_from_data_frame subcomponent
#' @export

getDependencies <- function(name, direction="in", graph=NULL, type=NULL, self=FALSE, ...) {
  if(is.null(graph)) graph <- suppressWarnings(getMadratGraph(...))
  packages <- c(graph$from_package,graph$to_package)
  names(packages) <- c(graph$from,graph$to)
  
  if(!(name %in% c(graph$from,graph$to))) {
    .tmp <- function(name, ...) {
      if(name %in% names(list(...))) {
        return(list(...)[[name]])
      } else {
        return(getConfig(name))
      }  
    }
    fpool <- getCalculations("read|calc|full|tool", packages = .tmp("packages", ...), globalenv = .tmp("globalenv", ...))
    fpool$shortcall <- sub("^.*:::","",fpool$call)
    if(!(name %in% fpool$shortcall))stop("There is no function with the name \"",name,"\"")
    if(!self) return(NULL) 
    return(data.frame(func = name, type = substr(name,1,4), package = fpool$package[fpool$shortcall == name],
                      row.names=NULL, stringsAsFactors = FALSE))
  }
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
  if(!self) tmp <- setdiff(tmp,name)
  
  out <- data.frame(func=tmp,type=substr(tmp,1,4),package=packages[tmp],row.names=NULL, stringsAsFactors = FALSE)
  if(!is.null(type)) out <- out[out$type %in% type,]
  out <- out[order(out$package),]
  out <- out[order(out$type),]
  return(data.frame(out ,row.names=NULL, stringsAsFactors = FALSE))
}

