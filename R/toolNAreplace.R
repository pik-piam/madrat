#' Tool: NA replace
#' 
#' Functions removes NAs, NaNs and infinite values in x and weight
#' 
#' 
#' @param x data
#' @param weight aggregation weight
#' @param replaceby value which should be used instead of NA. Either a single 
#' value or a MAgPIE object which can be expanded to the size of x (either same 
#' size or with lower dimensionality).
#' @param val.rm vector of values that should in addition be removed in x 
#' @return a list containing x and weight
#' @author Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}
#' @importFrom magclass magpie_expand is.magpie getCells getYears getNames
#' @export
#' 

toolNAreplace<-function(x, weight=NULL, replaceby=0, val.rm=NULL){
  if(is.magpie(replaceby)) {
    replaceby <- magpie_expand(replaceby,x)
    if(!all(dim(replaceby)==dim(x))) stop("replaceby must be expandable to the exact same size as x in order to work!")
    replaceby <- replaceby[getCells(x),getYears(x),getNames(x)]
  } else {
    if(length(replaceby)!=1) stop("replaceby has a length different than 1. It must be either a MAgPIE object or a single number!")
  }
  if(!is.null(val.rm)) x[x %in% val.rm] <- NA
  if(!is.null(weight)) {
    na_remove <- x*weight*0+1
  } else {
    na_remove <- x*0+1
  }
  x <- x*na_remove
  if(is.magpie(replaceby)) {
    x[is.na(x)]  <- replaceby[is.na(x)]
    x[is.nan(x)] <- replaceby[is.nan(x)]
  } else {
    x[is.na(x)]  <- replaceby
    x[is.nan(x)] <- replaceby
  }
  if(!is.null(weight)) {
    weight <- weight*na_remove
    weight[is.na(weight)]  <- 0
    weight[is.nan(weight)] <- 0
  }
  return(list(x=x,weight=weight))
}
