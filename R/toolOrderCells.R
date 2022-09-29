#' toolOrderCells
#'
#' reorder numbered spatial units (cells, clusters) by number. Function
#' will return the unmodified object, if the given subdimension does not exist
#' or does not contain cell information.
#'
#' @param x magclass object that should be ordered
#' @param dim subdimension which contains the cell information
#' @param na.rm boolean deciding how to deal with non-integer information
#' in cellular column. If FALSE, non-integer values will lead to a return
#' of the unsorted object, if TRUE non-integer cells will be removed from
#' the data set and the rest will get sorted
#'
#' @return ordered data in magclass format
#' @author Kristine Karstens, Jan Philipp Dietrich
#' @importFrom magclass dimExists
#' @export

toolOrderCells <- function(x, dim = 1.2, na.rm = FALSE) { #nolint
  if (!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  if (!dimExists(dim, x)) return(x)
  cells <- suppressWarnings(as.integer(getItems(x, dim = dim, full = TRUE)))
  if (!na.rm && anyNA(cells)) return(x)
  order <- robustOrder(cells)
  if (na.rm && anyNA(cells)) {
    # remove NAs
    order <- order[seq_len(length(order) - sum(is.na(cells)))]
  }
  out <- x[order, , ]
  getComment(out) <- c(getComment(x), paste0("Data reordered by spatial unit number (toolOrderCells): ", date()))
  return(out)
}
