#' Tool: NA replace
#' 
#' Functions removes NAs, NaNs and infinite values in x and weight
#' 
#' 
#' @param x data
#' @param weight aggregation weight
#' @param replaceby value which should be used instead of NA
#' @param val.rm vector of values that should in addition be removed in x 
#' @return a list containing x and weight
#' @author Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}
#' @export
#' 

toolNAreplace<-function(x, weight, replaceby=0, val.rm=NULL){
  if(!is.null(val.rm)) x[x %in% val.rm] <- NA
  na_remove<-x*weight*0+1
  x<-x*na_remove
  weight<-weight*na_remove
  x[is.na(x)]<-replaceby
  x[is.nan(x)]<-replaceby
  weight[is.na(weight)]<-0
  weight[is.nan(weight)]<-0
  return(list(x=x,weight=weight))
}