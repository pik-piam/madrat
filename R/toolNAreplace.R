#' Tool: NA replace
#' 
#' Functions removes NAs in x and weight
#' 
#' 
#' @param x data
#' @param weight aggregation weight
#' @param replaceby value which should be used instead of NA
#' @return a list containing x and weight
#' @author Benjamin Bodirsky
#' @seealso \code{\link{calcOutput}}
#' @export
#' 

toolNAreplace<-function(x,weight,replaceby=0){
  na_remove<-x*weight*0+1
  x<-x*na_remove
  weight<-weight*na_remove
  x[is.na(x)]<-replaceby
  x[is.nan(x)]<-replaceby
  weight[is.na(weight)]<-0
  weight[is.nan(weight)]<-0
  return(list(x=x,weight=weight))
}