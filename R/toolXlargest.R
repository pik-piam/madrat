#' @title toolXlargest
#' @description Selects the largest countries of a country-level magpie object
#'
#' @param range the position of the countries in the top X which should be returned.
#' @param type calcOutput function that shall be used for ranking
#' @param years range of years that shall be summed for ranking.  If NULL, the sum of all years is used.
#' @param elements range of elements that shall be summed for ranking. If NULL, all elements are used.
#' @param ... further parameters will be handed on to calcOutput function type.
#'
#' @return vector with ISO country codes
#' @author Benjamin Leon Bodirsky, Jan Philipp Dietrich
#' @export
#' @examples
#' 
#' \dontrun{ 
#' top10 <- toolXlargest(1:10, "TauTotal")
#' }
#' 
toolXlargest<-function(range=1:20,type="TauTotal",years=NULL , elements=NULL, ...){
  if (is.vector(type)){
    a<-calcOutput(type,aggregate = FALSE,years = years,...)  
  } else if(is.magpie(type)){
    a <- type
    if(!is.null(years)) a <- a[,years,]
  }
  if (!is.null(elements)){a<-a[,,elements]}
  out <- getRegions(sort(dimSums(a,dim=c(2,3)),decreasing = TRUE)[range])
  return(out)
}