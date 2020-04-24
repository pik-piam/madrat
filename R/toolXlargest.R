#' @title toolXlargest
#' @description Selects the largest countries of a country-level magpie object
#'
#' @param type magclass object that shall be used for ranking
#' @param range the position of the countries in the top X which should be returned.
#' @param years range of years that shall be summed for ranking.  If NULL, the sum of all years is used.
#' @param elements range of elements that shall be summed for ranking. If NULL, all elements are used.
#' @param ... further parameters will be handed on to calcOutput function type.
#'
#' @return vector with ISO country codes
#' @author Benjamin Leon Bodirsky, Jan Philipp Dietrich
#' @export
#' @examples
#' toolXlargest(population_magpie,range=1:3)
#' 
toolXlargest<-function(type, range=1:20, years=NULL , elements=NULL, ...){
  if (is.vector(type)){
    stop("This option is not available anymore as it violates madrat coding etiquette. Please provide the
         reference data directly as magclass object.")
  } else if(is.magpie(type)){
    a <- type
    if(!is.null(years)) a <- a[,years,]
  } else {
    stop("Unknown type input format.")
  }
  if (!is.null(elements)){a<-a[,,elements]}
  out <- getRegions(sort(dimSums(a,dim=c(2,3)),decreasing = TRUE)[range])
  return(out)
}