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
#' @export

getMadratInfo <- function(graph=NULL, cutoff=5, extended=FALSE, ...) {
  
  if(is.null(graph)) graph <- getMadratGraph(...)
  
  if(cutoff<0) cutoff <- 10^6
  
  message("\n.:: Check network size ::.")
  tmp <- graph[graph$from_package!="UNKNOWN",]
  # check number of nodes
  funcs <- unique(c(tmp$from,tmp$to))
  nread <- sum(grepl("^read",funcs))
  ncalc <- sum(grepl("^calc",funcs))
  nfull <- sum(grepl("^full",funcs))
  # check number of edges
  ncallread <- sum(grepl("^read",tmp$from))
  ncallcalc <- sum(grepl("^calc",tmp$from))
  nretrievecalls <- sum(grepl("^full",tmp$to))
  
  message("[INFO] ",nread," read functions (called ",ncallread," times, ",round(ncallread/nread,2)," calls on average)")
  message("[INFO] ",ncalc," calc functions (called ",ncallcalc," times, ",round(ncallcalc/ncalc,2)," calls on average)")
  message("[INFO] ",nfull," retrieve functions (triggering ",nretrievecalls," calls, ",round(nretrievecalls/nfull,2)," calls on average)")
  
  message("\n.:: Check readSource and calcOutput call syntax ::.") 
  fromNA <- (graph$from=="UNKNOWN")
  if(any(fromNA)) {
    message("[warning] following functions contain read or calc statements which could not be identified: \n[warning] -> ",
            paste(unique(graph$to[fromNA]),collapse="\n[warning] -> "),"\n[warning] Please adress the type explicitly in the call to allow",
            " for proper detection, e.g. readSource(\"MySource\")")
  } else {
    message("[passed] all calls could be interpreted!")
  }
  
  message("\n.:: Check function availability ::.") 
  from_packageNA <- (graph$from_package=="UNKNOWN") & !fromNA
  if(any(from_packageNA)) {
    graph$from_package[from_packageNA] <- "UNKNOWN"
    message("[warning]\n[warning] Following functions could not be found in the scope of packages to be checked.: \n[warning]   ",
            paste0(graph$from[from_packageNA]," -> ",graph$to[from_packageNA],collapse="\n[warning]   "),
            "\n[warning]\n[warning] Please make sure that they exist and adjust the scope of packages accordingly!")
  } else {
    message("[passed] all read and calc functions found in the given scope!") 
  }
  
  message("\n.:: Check for bidirectional package connections ::.")
  pkgdeps <- unique(graph[c("from_package","to_package")])
  pkgdeps <- pkgdeps[pkgdeps$from_package!=pkgdeps$to_package & pkgdeps$from_package!="UNKNOWN",]
  if(nrow(pkgdeps)>2) {
    din <- paste0(pkgdeps$from_package,"-",pkgdeps$to_package)
    dout <- paste0(pkgdeps$to_package,"-",pkgdeps$from_package)  
    if(any(din %in% dout)) {
      p <- pkgdeps[din %in% dout,]
      hints <- NULL
      hints_full <- list()
      message("[warning]\n[warning] bidirectional dependencies found: \n[warning]   -> ",
              paste0(p$from_package," -> ",p$to_package,collapse="\n[warning]   -> "))
      message("[warning]\n[warning] The following links between these packages exist:")
      for(i in 1:nrow(p)) {
        tmp <- graph[graph$from_package==p$from_package[i] & graph$to_package==p$to_package[i],]
        hints_full[[i]] <- paste0(tmp$from," -> ",tmp$to)
        message("[warning]\n[warning]   .: PACKAGES | ",paste0(p$from_package[i]," -> ",p$to_package[i])," :.")
        if(length(hints_full[[i]])>cutoff) {
          hints_short <- c(hints_full[[i]][1:cutoff],"...")
        } else {
          hints_short <- hints_full[[i]]
        }
        message("[warning]     -> ",paste0(hints_short,collapse="\n[warning]     -> "))
        if(nrow(tmp)<5 & nrow(tmp)>0) hints <- paste0(hints,paste0(tmp$from,"->",tmp$to,collapse=", "), collapse=", ") 
      }
    } else {
      message("[passed] no bidirectional package connections found!")
    }
  } else {
    message("[passed] no bidirectional package connections found!")
  }
  
  #### Further Info is based on graph structure ###
  if (!requireNamespace("igraph", quietly = TRUE)) {
    message("\n[INFO] Package \"igraph\" needed for additional information.")
  } else {
  
    ggraph <- igraph::graph_from_data_frame(graph)
    
    writeCommunities <- function(membership, what="independent networks", elem="network", cutoff=5){
      no <- max(membership)
      message("[INFO] ",no," ",what," detected")
      for(i in 1:no) {
        member <- names(membership)[membership==i]
        message("[INFO]\n[INFO] .: ",elem," #",i," (",length(member)," members) :.")
        if(length(member)>cutoff) member <- c(member[1:cutoff],"...")
        message("[INFO]  -> ",paste(member,collapse="\n[INFO]  -> "))
      }
    }
    
    message("\n.:: Check for independent networks ::.")
    comp <- igraph::components(ggraph)
    writeCommunities(comp$membership, what="independent networks", 
                     elem="network", cutoff=cutoff)
      
    
    message("\n.:: Check for sub-structures ::.")
    fullfunc <- grep("^full",attr(igraph::V(ggraph),"names"),value=TRUE)
    greduced <- igraph::delete.vertices(ggraph,fullfunc)
    comp <- igraph::components(greduced)
    writeCommunities(comp$membership, what="independent calculations cluster", 
                     elem="cluster", cutoff=cutoff)
    
    if(extended) {
      message("\n.:: Check for community structures ::.")
      ceb <- igraph::cluster_edge_betweenness(igraph::as.undirected(ggraph))
      writeCommunities(igraph::membership(ceb), what="close communities", 
                       elem="community", cutoff=cutoff)
    }
    
  }
}

