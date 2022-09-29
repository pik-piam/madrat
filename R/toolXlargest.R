#' @title toolXlargest
#' @description Selects the countries with the highest values in a magpie object
#'
#' @param x magclass object that shall be used for ranking
#' @param range the position of the countries in the top X which should be returned.
#' @param years range of years that shall be summed for ranking.  If NULL, the sum of all years is used.
#' @param elements range of elements that shall be summed for ranking. If NULL, all elements are used.
#' @param ... further parameters will be handed on to calcOutput function type.
#'
#' @return vector with ISO country codes
#' @author Benjamin Leon Bodirsky, Jan Philipp Dietrich
#' @export
#' @examples
#' toolXlargest(magclass::maxample("pop"), range = 1:3)
toolXlargest <- function(x, range = 1:20, years = NULL, elements = NULL, ...) {
  if (!is.magpie(x)) stop("Input must be a MAgPIE object!")
  if (!is.null(years)) x <- x[, years, ]
  if (!is.null(elements)) x <- x[, , elements]
  tmp <- dimSums(x, dim = c(2, 3))
  out <- out <- getItems(tmp[robustOrder(tmp, decreasing = TRUE), , ], dim = 1.1)[range]
  return(out)
}
