#' visualizeDependencies
#'
#' Creates a graphical visualization of dependencies between functions in the mr-universe.
#'
#' @param functions function or vector of functions to be analyzed
#' @param direction Character string, either “in”, “out” or "both". If “in” all sources
#' feeding into the function are listed. If “out” consumer of the function are listed.
#' If “both” the union of "in" and "out" is returned.
#' @param order order of dependencies. Order 1 would be only functions directly called from (in case
#' of direction "in") or directly calling (in case of direction "out") are shown. Order 2 will also
#' show direct dependencies of the order 1 dependencies, order 3 also the direct dependencies from
#' order 2 dependencies, etc.
#' @param packages packages to use when searching dependencies
#' @param filename If a filename is provided, the resulting graph will be saved
#' @author Debbora Leip
#' @seealso \code{\link{getDependencies}}, \code{\link{getMadratGraph}}, \code{\link{getMadratInfo}}
#' @importFrom igraph make_ego_graph ego V V<- union
#' @export


visualizeDependencies <- function(functions, direction = "both", order = 2, #nolint
                                  packages = getConfig("packages"), filename = NULL) {

  # check for required packages
  if (!requireNamespace("graphics", quietly = TRUE)) {
    stop("Package \`graphics\` required to visualize dependencies")
  }
  if (!is.null(filename) && !requireNamespace("grDevices", quietly = TRUE)) {
    stop("Package \`grDevices\` required to save dependency graph")
  }

  # get next order of neighbors
  nextOrder <- function(graph, nodes, mode) {
    egoGraph <- make_ego_graph(graph, nodes, order = 1, mode = mode)
    egoVertices <- do.call(c, ego(graph, nodes, order = 1, mode = mode))
    return(list(egoGraph, egoVertices))
  }

  # combine list of graphs
  combineGraphs1 <- function(graphs) {
    graph <- graphs[[1]]
    if (length(graphs) > 1) {
      for (i in 2:length(graphs))
        graph <- igraph::union(graph, graphs[[i]])
    }
    return(graph)
  }

  # combine list of new graphs to original graph
  combineGraphs2 <- function(graph, tmp) {
    for (j in seq_along(tmp)) {
      graph <- igraph::union(graph, tmp[[j]])
    }
    return(graph)
  }

  # get all orders in one direction
  oneDirection <- function(order, functions, graphMadrat, mode) {
    allGraphs <- list()
    ind <- 1

    # find dependencies up to given order in both directions
    for (func in functions) {

      # direct neighbors incoming
      current <- nextOrder(graphMadrat, func, mode)
      graphTmp <- current[[1]][[1]]

      # higher order incoming neighbors recursively
      if (order > 1) {
        for (i in 2:order) {
          if (length(current[[2]]) > 0) {
            current <- nextOrder(graphMadrat, current[[2]], mode)
            graphTmp <- combineGraphs2(graphTmp, current[[1]])
          }
        }
      }

      # adding graphs of current central vertex (func) to lists
      allGraphs[[ind]] <- graphTmp
      ind <- ind + 1
    }
    return(combineGraphs1(allGraphs))
  }

  #################

  # get full graph
  dfGraphMadrat <- getMadratGraph(packages = packages)
  graphMadrat <- graph_from_data_frame(dfGraphMadrat)

  # functions as list
  if (typeof(functions) == "character") {
    functions <- as.list(functions)
  }

  # check that all functions to be analyzed are in the given packages
  if (!all(functions %in% V(graphMadrat)$name)) {
    stop(paste0("The function(s) ", paste0(functions[!(functions %in% V(graphMadrat)$name)], collapse = ", "),
                " cannot be found in the package(s) you are analyzing (", paste0(packages, collapse = ", "), ")."))
  }

  # find dependencies up to given order in necessary directions
  if (direction == "both") {
    fullGraphIn <- oneDirection(order, functions, graphMadrat, "in")
    fullGraphOut <- oneDirection(order, functions, graphMadrat, "out")
    # combine graphs
    fullGraph <- igraph::union(fullGraphIn, fullGraphOut)
    # make incoming vertices squares in plot, outgoing circles
    V(fullGraph)$shape[V(fullGraph)$name %in% V(fullGraphIn)$name] <- "square"
    V(fullGraph)$shape[!(V(fullGraph)$name %in% V(fullGraphIn)$name)] <- "circle"
  } else if (direction == "in") {
    fullGraph <- oneDirection(order, functions, graphMadrat, "in")
    V(fullGraph)$shape <- "square"
  } else {
    fullGraph <- oneDirection(order, functions, graphMadrat, "out")
    V(fullGraph)$shape <- "circle"
  }


  # add corresponding mr package to each vertex of graph
  for (i in seq_along(V(fullGraph))) {
    V(fullGraph)$color[i] <- ifelse(V(fullGraph)$name[i] %in% dfGraphMadrat$from,
                                  dfGraphMadrat$from_package[which(dfGraphMadrat$from == V(fullGraph)$name[i])[1]],
                                  dfGraphMadrat$to_package[which(dfGraphMadrat$to == V(fullGraph)$name[i])[1]])
  }

  # all packages that are relevant
  pkg <- unique(V(fullGraph)$color)

  # set default colors
  colorsIn <- c("deepskyblue", "gold", "mediumpurple", "#33cbb9", "#219421")

  # set vertex colors according to package (different shades for incoming/outgoing)
  for (i in seq_along(pkg)) {
    V(fullGraph)$color[which(V(fullGraph)$color == pkg[i])] <- colorsIn[i]
  }

  # make central vertices red
  V(fullGraph)$color[V(fullGraph)$name %in% functions] <- "#f84141"
  V(fullGraph)$shape[V(fullGraph)$name %in% functions] <- "sphere"

  # plot graph
  if (!is.null(filename)) grDevices::png(filename, 800, 800)
  plot(fullGraph)
  graphics::legend("topright", legend = c(pkg, "Central functions"), pch = 16,
                                col = c(colorsIn[seq_along(pkg)], "#c93535"), bty = "n")
  if (direction == "both") graphics::legend("bottomright", legend = c("In", "Out"), pch = c(22, 21), bty = "n")
  if (direction == "in") graphics::legend("bottomright", legend = c("In"), pch = 22, bty = "n")
  if (direction == "out") graphics::legend("bottomright", legend = c("Out"), pch = 21, bty = "n")
  if (!is.null(filename)) grDevices::dev.off()

  # return graph and list of relevant packages
  return(list(fullGraph, pkg))
}