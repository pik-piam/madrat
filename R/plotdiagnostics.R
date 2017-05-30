#' Diagnostics plot
#' 
#' Function that plot diagnostic data from a madrat run and shows the network of function executions 
#' and distribution of time consumption
#' 
#' 
#' @param filename path to a diagnostics file as created with the setConfig argument diagnostics
#' @param scale defines how time should scale the size of the vertices in the network. Available options
#' are linear (lin), logarithmic (log) and square-root (sqrt).
#' @param cumulate_time if set to TRUE the size of each node will reflect the cumulated execution time of the corresponding 
#' function (including the execution time of all underlying functions). If set to FALSE it will only show the net time
#' spend in this function (excluding underlying functions).
#' @param png png file name to save the plot as a png. If !NULL the function will wait until the interactive plot window will be closed
#' and will then print the picture as it was in this window (still experimental!).
#' @export
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{toolstartmessage}}, \code{\link{setConfig}}
#' @examples
#' 
#' \dontrun{
#' plotDiagnostics("diagnostics.csv")
#' }
#'
#' @importFrom utils read.table
#' @importFrom grDevices dev.off
#' @importFrom methods is


plotDiagnostics <- function(filename, scale="sqrt", cumulate_time=FALSE, png=NULL) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  x <- read.table(filename,stringsAsFactors = TRUE, sep=";", header=TRUE, quote = "")

  fillIDs <- function(x) {
   if("id" %in% colnames(x)) {
     #replace "none" values (if possible)
     for(i in which(x[["id"]]=="none")) {
      tmp <- setdiff(x[x[i,"functioncall"]==x[,"functioncall"],"id"],"none")
      if(length(tmp)>0) {
        x[i,"id"] <- tmp[1]
      }
     }
    
     #Upgrade all IDs to the longest id version they are a subset of
     l <- levels(x[["id"]])
     for(i in 1:length(l)) {
       tmp <- grep(l[i],l,value=TRUE)
       if(length(tmp)>1) {
         l[i] <- tmp[(nchar(tmp)==max(nchar(tmp)))][1]
       }
     }
     levels(x[["id"]]) <- l
     
     #replace IDs with corresponding functioncall statements
     l <- levels(x[["id"]])
     for(i in 1:length(l)) {
       if(l[i]!="none") {
         l[i] <- as.character(x[["functioncall"]][x[["id"]]==l[i]][1])
       }
     }
     levels(x[["id"]]) <- l
     
     #replace "none" with functioncall
     x[["id"]] <- as.character(x[["id"]])
     x[["id"]][x[["id"]]=="none"] <- as.character(x[["functioncall"]])[x[["id"]]=="none"]
     
   } else {
     x <- cbind(x,id=as.character(x[["functioncall"]]),stringsAsFactors=FALSE)
   }
   return(x)
  }
  x <- fillIDs(x)
  
  factor2character <- function(x) {
    if(is.factor(x)) return(as.character(x))
    else return(x)
  }
  x[] <- lapply(x, factor2character)
  
  if(cumulate_time) {
    runtime <- x[!x[,"start"],"runtime"]
    names(runtime) <- x[!x[,"start"],"functioncall"]
  } else {
    #add level information
    addlevelinformation <- function(x) {
      x$level <- 0
      for(i in 2:(nrow(x)-1)) {
        if(x$start[i]) {
          x$level[i] <- x$level[i] + 1
          x$level[i+1] <- x$level[i]
        } else {
          x$level[i+1] <- x$level[i] - 1
        }
      }
      return(x)
    }
    x <- addlevelinformation(x)
    
    fcalls <- unique(x[!x[,"start"],"functioncall"])
    
    xgrep <- function(f,x) {
      # return indices of everything executed within the first
      # execution of f
      tmp <- grep(f,x$functioncall,fixed=TRUE)[1:2]
      xx <- x[tmp[1]:tmp[2],][x$level[tmp[1]:tmp[2]]==x$level[tmp[1]]+1,]
      rt <- sum(xx[xx$start==FALSE,]$runtime)
      return(x$runtime[tmp[2]]-rt)
    }
    
    runtime <- sapply(fcalls,xgrep,x)
    if(any(runtime<0)) {
      warning("Runtimes < 0 calculated. This can happen if the diagnostics file contains data from more than one, incomplete run. Negative values are set to 0.")
      runtime[runtime<0] <- 0
    }
  }
  
  parent <- NULL

  edgelist <- matrix(nrow = 0, ncol=2, dimnames = list(NULL,c("from","to")))

  for(i in 1:nrow(x)) {
    if(x[i,"start"]) {
      if(!is.null(parent)) {
        edgelist <- rbind(edgelist,c(x[i,"id"],parent[1]))
      } 
      parent <- c(x[i,"id"],parent)
    } else {
      if(length(parent)>1) {
        parent <- parent[2:length(parent)]
      } else {
        parent <- NULL
      }
    }
  }

  #remove duplicates
  edgelist <- edgelist[!duplicated(paste(edgelist[,"from"],edgelist[,"to"])),]

  g <- igraph::graph.edgelist(edgelist)
  col <- c(retrieveData="red", calcOutput="orange", readSource="yellow")
  igraph::V(g)$color <- col[gsub("\\(.*$","",igraph::V(g)$name)]
  igraph::V(g)$label <- gsub('^[^"]*"([^"]*)".*$',"\\1",gsub(",.*$","",igraph::V(g)$name))
  igraph::V(g)$label.color <- "black"
  
  tmp <- runtime[igraph::V(g)$name]
  tmp[is.na(tmp)] <- 0
  
  if(scale=="log") {
    igraph::V(g)$size <- (log(tmp+1) + 1)*3
  } else if(scale=="lin") {
    igraph::V(g)$size <- tmp/max(tmp)*100
  } else if(scale=="sqrt") {
    igraph::V(g)$size <- sqrt(tmp)/sqrt(max(tmp))*100
  } else {
    stop("Unknown scale. Please use squreroot (sqrt), logarithmic (log) or linear (lin) instead.")
  }
  
  id <- igraph::tkplot(g)
  if(!is.null(png)) {
    repeat {
      tmp <- try(igraph::tk_coords(id),silent = TRUE)
      if(is(tmp,"try-error")) break
      coord <- tmp
    }
    png(filename=png,width=1000,height=1000)
    igraph::plot.igraph(g,layout=coord)
    dev.off()
  } 
  
  return(g)
}