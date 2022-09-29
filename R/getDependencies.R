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

getDependencies <- function(name, direction = "in", graph = NULL, type = NULL, self = FALSE, ...) {
  if (is.null(graph)) graph <- getMadratGraph(...)
  packages <- c(graph$from_package, graph$to_package)
  names(packages) <- c(graph$from, graph$to)

  .filterList <- function(l, calls, owncall, aggregate = "monitor") {
    # in case of aggregate, aggregate list entries over all calls
    # otherwise only take entries from current call
    .tmp <- function(x, filter) return(robustSort(unique(unlist(x[filter]))))
    aggr <- (names(l) %in% aggregate)
    out  <- l
    out[aggr] <- lapply(l[aggr], .tmp, filter = union(owncall, calls))
    out[!aggr] <- lapply(l[!aggr], .tmp, filter = owncall)
    if (all(vapply(out, length, integer(1)) == 0)) return(NULL)
    return(out)
   }

  fpool <- attr(graph, "fpool")
  fpool$shortcall <- sub("^.*:::", "", fpool$call)
  owncall <- fpool$call[fpool$shortcall == name]

  if (!(name %in% c(graph$from, graph$to))) {
    if (!(name %in% fpool$shortcall)) stop("There is no function with the name \"", name, "\"")
    if (!self) return(NULL)
    out <- data.frame(func = name, type = substr(name, 1, 4), package = fpool$package[fpool$shortcall == name],
                      call = owncall,
                      hash = attr(graph, "hash")[fpool$call[fpool$shortcall == name]],
                      row.names = NULL, stringsAsFactors = FALSE)
    attr(out, "mappings") <- robustSort(unique(unlist(attr(graph, "mappings")[out$call])))
    attr(out, "flags") <- .filterList(attr(graph, "flags"), owncall, owncall)
    return(out)
  }
  ggraph <- igraph::graph_from_data_frame(graph)
  if (direction == "full") direction <- "all"
  if (direction == "both") {
    tmp <- unique(c(attr(igraph::subcomponent(ggraph, name, "in"), "names"),
                    attr(igraph::subcomponent(ggraph, name, "out"), "names")))
  } else if (direction == "dout") {
    tmp <- robustSort(unique(graph$to[graph$from == name]))
  } else if (direction == "din") {
    tmp <- robustSort(unique(graph$from[graph$to == name]))
  } else {
    tmp <- attr(igraph::subcomponent(ggraph, name, direction), "names")
  }
  if (!self) tmp <- setdiff(tmp, name)

  out <- data.frame(func = tmp, type = substr(tmp, 1, 4), package = packages[tmp], row.names = NULL,
                    stringsAsFactors = FALSE)
  if (nrow(out) > 0) out$call <- paste0(out$package, ":::", out$func)
  out$call[out$package == ".GlobalEnv"] <- out$func[out$package == ".GlobalEnv"]
  out$hash <- attr(graph, "hash")[out$call]
  if (!is.null(type)) out <- out[out$type %in% type, ]
  out <- out[robustOrder(out$package), ]
  out <- out[robustOrder(out$type), ]
  out <- data.frame(out, row.names = NULL, stringsAsFactors = FALSE)
  attr(out, "mappings") <- robustSort(unique(unlist(attr(graph, "mappings")[out$call])))
  attr(out, "flags") <- .filterList(attr(graph, "flags"), out$call, owncall)
  return(out)
}
