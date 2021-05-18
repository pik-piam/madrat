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
  out <- x[robustOrder(as.integer(getItems(x, dim=1.2))),,]
  getComment(out) <- c(getComment(x), paste0("Data reordered by spatial unit number (toolOrderCells): ",date()))
  return(out)
}

