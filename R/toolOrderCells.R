#' toolOrderCells
#'
#' reorder numbered spatial units (cells, clusters) by number
#' 
#' @param x magclass object that should be ordered
#' 
#' @return ordered data in magclass format
#' @author Kristine Karstens
#'
#' @export

toolOrderCells <- function(x){
  
  if(!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  if(length(getItems(x, split=TRUE)[[1]])!=2 | any(!grepl("[0-9]", getCells(x)))) return(x)
  
  if (withMetadata() && !is.null(getOption("calcHistory_verbosity")) && getOption("calcHistory_verbosity")>1) {
    if (object.size(sys.call()) < 5000 && as.character(sys.call())[1]=="toolOrderCells")  calcHistory <- "update"
    #Special calcHistory handling necessary for do.call(x$aggregationFunction,x$aggregationArguments) from calcOutput
    else  calcHistory <- paste0("toolOrderCells(x=unknown")
  } else  calcHistory <- "copy"

  out <- x[order(as.integer(getItems(x, dim=1.2))),,]
  
  getComment(out) <- c(getComment(x), paste0("Data reordered by spatial unit number (toolOrderCells): ",date()))
  return(updateMetadata(out,unit="copy",calcHistory=calcHistory))
}

