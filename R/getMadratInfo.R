#' getMadratInfo
#'
#' Collects and returns detailed information about the currently loaded
#' network of madrat functions.
#'
#'
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}.
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param cutoff Integer introducing a cutoff of items to be returned for
#' outputs which can become quite verbose.
#' @param extended Will add additional outputs which has been removed
#' from standard output due to limited usefulness.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCalculations}}, \code{\link{getMadratGraph}}
#' @importFrom igraph graph_from_data_frame components V
#' cluster_edge_betweenness as_undirected membership
#' @export

getMadratInfo <- function(graph = NULL, cutoff = 5, extended = FALSE, ...) {
  out <- list()

  if (is.null(graph)) graph <- getMadratGraph(...)

  if (cutoff < 0) cutoff <- 10^6

  message("\n.:: Check network size ::.")
  tmp <- graph[graph$from_package != "UNKNOWN", ]
  # check number of nodes
  funcs <- attr(graph, "fpool")$fname
  nread <- sum(grepl("^read", funcs))
  ncalc <- sum(grepl("^calc", funcs))
  nfull <- sum(grepl("^full", funcs))
  ntool <- sum(grepl("^tool", funcs))
  # check number of edges
  ncallread <- sum(grepl("^read", tmp$from))
  ncallcalc <- sum(grepl("^calc", tmp$from))
  nretrievecalls <- sum(grepl("^full", tmp$to))
  ncalltool <- sum(grepl("^tool", tmp$from))

  message(
    "[INFO] ", nread, " read functions (called ", ncallread, " times, ", round(ncallread / nread, 2),
    " calls on average)"
  )
  message(
    "[INFO] ", ncalc, " calc functions (called ", ncallcalc, " times, ", round(ncallcalc / ncalc, 2),
    " calls on average)"
  )
  message(
    "[INFO] ", ntool, " tool functions (called ", ncalltool, " times, ", round(ncalltool / ntool, 2),
    " calls on average)"
  )
  message(
    "[INFO] ", nfull, " retrieve functions (triggering ", nretrievecalls, " calls, ",
    round(nretrievecalls / nfull, 2), " calls on average)"
  )

  message("\n.:: Check readSource and calcOutput call syntax ::.")
  fromNA <- (graph$from == "UNKNOWN")
  if (any(fromNA)) {
    warning("Some read- or calc- functions could not be identified! Check report for more info!")
    message(
      "[warning] following functions contain read or calc statements which could not be identified: \n[warning] -> ",
      paste(unique(graph$to[fromNA]), collapse = "\n[warning] -> "),
      "\n[warning] Please adress the type explicitly in the call to allow",
      " for proper detection, e.g. readSource(\"MySource\")"
    )
  } else {
    message("[passed] all calls could be interpreted!")
  }

  message("\n.:: Check function availability ::.")
  fromPackageNA <- (graph$from_package == "UNKNOWN") & !fromNA
  if (any(fromPackageNA)) {
    graph$from_package[fromPackageNA] <- "UNKNOWN"
    warning("Some functions could not be found in scope! Check report for more info!")
    message(
      paste0(
        "[warning]\n[warning] Following functions could not be found in the scope of packages to be ",
        "checked.: \n[warning]   "
      ),
      paste0(graph$from[fromPackageNA], " -> ", graph$to[fromPackageNA], collapse = "\n[warning]   "),
      "\n[warning]\n[warning] Please make sure that they exist and adjust the scope of packages accordingly!"
    )
  } else {
    message("[passed] all read and calc functions found in the given scope!")
  }

  message("\n.:: Check for bidirectional package connections ::.")
  .checkBidirectional(graph, details = TRUE, cutoff = cutoff)

  message("\n.:: Check for read/calc calls in tool functions ::.")
  tmp <- robustSort(unique(graph[grepl("^tool", graph$to) & !grepl("^tool", graph$from), "to"]))
  if (length(tmp) > 0) {
    warning("Some tool functions contain read or calc statements! Check report for more info!")
    message(
      "[warning]\n[warning] Following tool function contain either read or calc calls: \n[warning]   ",
      paste0(tmp, collapse = "\n[warning]   "),
      "\n[warning]\n[warning] Please remove these calls from all tool functions!"
    )
  } else {
    message("[passed] no read/call calls in tool functions found!")
  }

  message("\n.:: Check for unused functions (tools ignored) ::.")
  tmp <- setdiff(grep("^(full|tool)", attr(graph, "fpool")$fname, invert = TRUE, value = TRUE), graph$from)
  if (length(tmp) > 0) {
    message(
      "[INFO]\n[INFO] No calls found for the following ", length(tmp), " functions: \n[INFO]   ",
      paste0(tmp, collapse = "\n[INFO]   "),
      "\n[INFO]\n[INFO] Are these functions still needed?"
    )
  } else {
    message("[INFO] no unused functions found!")
  }

  #### Further Info is based on graph structure ###
  # create graph but without tool functions
  ggraphNoTools <- igraph::graph_from_data_frame(graph[!grepl("^tool", graph$from), ])

  writeCommunities <- function(membership, what = "independent networks", elem = "network", cutoff = 5) {
    no <- max(membership)
    out <- list()
    message("[INFO] ", no, " ", what, " detected")
    for (i in seq_len(no)) {
      member <- names(membership)[membership == i]
      message("[INFO]\n[INFO] .: ", elem, " #", i, " (", length(member), " members) :.")
      out[[i]] <- member
      if (length(member) > cutoff) member <- c(member[seq_len(cutoff)], "...")
      message("[INFO]  -> ", paste(member, collapse = "\n[INFO]  -> "))
    }
    return(out)
  }

  message("\n.:: Check for independent networks (tools ignored) ::.")
  comp <- igraph::components(ggraphNoTools)
  out$independent_networks <- writeCommunities(comp$membership, # nolint
    what = "independent networks",
    elem = "network", cutoff = cutoff
  )


  message("\n.:: Check for sub-structures (tools ignored) ::.")
  fullfunc <- grep("^full", attr(igraph::V(ggraphNoTools), "names"), value = TRUE)
  greduced <- igraph::delete_vertices(ggraphNoTools, fullfunc)
  comp <- igraph::components(greduced)
  out$sub_structures <- writeCommunities(comp$membership, # nolint
    what = "independent calculations cluster",
    elem = "cluster", cutoff = cutoff
  )

  message("\n.:: Identify functions which are used exclusively by one full function (tools ignored) ::.")
  funcs <- attr(graph, "fpool")$fname
  fulls <- grep("^full", funcs, value = TRUE)
  tmpfun <- function(x, graph = graph) {
    return(data.frame(callfunc = x, getDependencies(x, graph = graph), stringsAsFactors = FALSE))
  }
  tmp <- do.call(rbind, lapply(fulls, tmpfun, graph = graph))
  n <- table(tmp$func)
  nouse <- setdiff(funcs, tmp$func)
  nouse <- grep("^(full|tool)", nouse, value = TRUE, invert = TRUE)
  multiuse <- names(n)[n > 1]
  singleuse <- names(n)[n == 1]
  singleuse <- tmp[match(singleuse, tmp$func), ]

  out$exclusive_use <- list() # nolint

  out$exclusive_use$multi_use <- multiuse # nolint

  for (f in fulls) {
    tmp <- singleuse[singleuse$callfunc == f, 2:3]
    tmp <- tmp[robustOrder(tmp[[2]], tmp[[1]]), ]
    message("[INFO]\n[INFO] .: exclusive calls for ", f, " (", dim(tmp)[1], " members) :.")
    out$exclusive_use[[f]] <- tmp
    if (dim(tmp)[1] > cutoff) tmp <- rbind(tmp[seq_len(cutoff), ], c("...", "..."))
    if (dim(tmp)[1] > 0) message("[INFO]  -> ", paste(tmp$package, tmp$func, sep = "::", collapse = "\n[INFO]  -> "))
  }

  tmp <- data.frame(as.matrix(attr(graph, "fpool")[, c("fname", "package")]), stringsAsFactors = FALSE)
  tmp <- tmp[tmp$fname %in% nouse, ]
  tmp <- tmp[robustOrder(tmp[[2]], tmp[[1]]), ]
  message("[INFO]\n[INFO] .: functions with no calls in full-functions (", dim(tmp)[1], " members) :.")
  out$exclusive_use$no_use <- tmp # nolint
  if (dim(tmp)[1] > cutoff) tmp <- rbind(tmp[seq_len(cutoff), ], c("...", "..."))
  if (dim(tmp)[1] > 0) message("[INFO]  -> ", paste(tmp$package, tmp$fname, sep = "::", collapse = "\n[INFO]  -> "))

  if (extended) {
    message("\n.:: Check for community structures (tools ignored) ::.")
    ceb <- igraph::cluster_edge_betweenness(igraph::as_undirected(ggraphNoTools))
    out$community_structrues <- writeCommunities(igraph::membership(ceb), # nolint
      what = "close communities",
      elem = "community", cutoff = cutoff
    )
  }

  invisible(out)
}
