#' get official ISO list
#' 
#' Function which returns the ISO list which is used as default for the input
#' data preparation. It contains the countries to which all source data has to
#' be up- or downscaled to.
#' 
#' 
#' @param type Determines what countries should be returned. "all" returns all
#' countries, "important" returns all countries which are above the population
#' threshold set in the configuration and "dispensable" returns all countries
#' which are below the threshold.
#' @return vector of default ISO country codes.
#' @note Please always use this function instead of directly referring to the data
#' object as the format in this data list might change in the future!
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getSources}}, \code{\link{getCalculations}}
#' @examples
#' 
#' getISOlist()
#' getISOlist("dispensable")
#' 
#' @importFrom magclass ncells
#' @export
getISOlist <- function(type="all") {
  iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "madrat"),row.names=NULL)
  iso_country1<-as.vector(iso_country[,"x"])
  names(iso_country1)<-iso_country[,"X"]
  if(type=="all") {
    return(sort(iso_country1))
  } else {
    ref <- sort(iso_country1)
  }
  pop2015 <- read.magpie(system.file("extdata","pop2015.csv",package = "madrat"))
  names(dimnames(pop2015))<-c("ISO2_WB_CODE","Year","data")
  if(length(ref)!=ncells(pop2015))  stop("Inconsistency between official ISO list and pop2015 data set (different number of countries)")
  if(!all(ref==getCells(pop2015))) stop("Inconsistency between official ISO list and pop2015 data set (different countries)")
  
  if(type=="important") {
    return(ref[as.vector(pop2015[,1,1]>=getConfig("pop_threshold")/10^6)])
  } else if(type=="dispensable") {
    return(ref[as.vector(pop2015[,1,1]<getConfig("pop_threshold")/10^6)])
  } else {
    stop("Unknown type",type)
  }
}
