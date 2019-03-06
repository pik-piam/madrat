#' Tool: CountryFill
#' 
#' This function expects a MAgPIE object with ISO country codes in the spatial
#' dimension. These ISO codes are compared with the official ISO code country
#' list (stored as supplementary data in the madrat package). If there is an
#' ISO code in the data but not in the official list this entry is removed, if
#' an entry of the official list is missing in the data this entry is added and
#' set to the value of the argument fill.
#' 
#' 
#' @param x MAgPIE object with ISO country codes in the spatial dimension
#' @param fill Number which should be used for filling the gaps of missing
#' countries.
#' @param no_remove_warning A vector of non-ISO country codes that exist in the
#' data and that should be removed by CountryFill but without creating a
#' warning (they will be removed in any case). You should use that argument if
#' you are certain that the given entries should be actually removed from the
#' data.
#' @param overwrite logical deciding whether existing data should be overwritten,
#' if there is a specific mapping provided for that country, or not
#' @param verbosity verbosity for information about filling important countries.
#' 0 = warning will show up (recommended if filling of important countries is not expected), 
#' 1 = note will show up in reduced log file (default),
#' 2 = info will show up in extended log file (recommended if filling of important countries
#' is not critical and desired).
#' @param ... Mappings between countries for which the data is missing and 
#' countries from which the data should be used instead for these countries 
#' (e.g. "HKG"="CHN" if HongKong should receive the value of China). This 
#' replacement usually only makes sense for intensive values.
#' @return A MAgPIE object with spatial entries for each country of the
#' official ISO code country list.
#' @author Jan Philipp Dietrich
#' @examples
#' 
#' library(magclass)
#' x <- new.magpie("DEU",1994,"bla",0)
#' y <- toolCountryFill(x,99)
#' 
#' @importFrom magclass getRegions new.magpie getYears getNames mbind setCells
#' @export 
toolCountryFill <- function(x,fill=NA, no_remove_warning=NULL, overwrite=FALSE, verbosity=1, ...) {
  iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "madrat"),row.names=NULL)
  iso_country1<-as.vector(iso_country[,"x"])
  names(iso_country1)<-iso_country[,"X"]
  missing_countries <- setdiff(iso_country1,getRegions(x))
  additional_countries <- setdiff(getRegions(x),iso_country1)
  
  #remove unrequired data
  if(length(additional_countries)>0) {
    x <- x[setdiff(getRegions(x),additional_countries),,]
    #warn only for countries which were not explicitly mentioned in argument "remove"
    countries2warn <- setdiff(additional_countries,no_remove_warning)
    if(length(countries2warn)>0) vcat(0,"Data for following unknown country codes removed: ",paste(countries2warn,collapse=", "))
  }
  
  #check mappings
  map <- c(...)
  if(!is.null(map)) {
    if(any(map %in% missing_countries)) {
      tmp <- map[map %in% missing_countries]
      stop("Try to fill a country with data from another country which is non-existent in the data set (",paste(tmp,names(tmp),sep=" -> ",collapse=" | "),").") 
    }
    if(!all(names(map) %in% missing_countries)) {
      if(overwrite) {
        tmp <- map[!(names(map) %in% missing_countries)]
        vcat(1,"Existing data overwritten with data from another country (",paste(tmp,names(tmp),sep=" -> ",collapse=" | "),").") 
      } else {
        map <- map[(names(map) %in% missing_countries)]
        if(length(map)==0) map <- NULL
      }
    }
  }
  
  #add missing data
  if(length(missing_countries)>0) {
    
    missing_important_countries <- setdiff(intersect(missing_countries,getISOlist("important")),names(map))
    if(length(missing_important_countries)>0) {
      names_countries <- names(iso_country1)[iso_country1 %in% missing_important_countries]
      vcat(verbosity," - toolCountryFill set missing values for IMPORTANT countries to ",fill,":")
      vcat(verbosity,paste(" --- ",names_countries,paste0("(",iso_country1[names_countries],")"),sep=""),sep="")
    }
    
    missing_dispensable_countries <- setdiff(intersect(missing_countries,getISOlist("dispensable")),names(map))
    if(length(missing_important_countries)>0) {
      names_countries <- names(iso_country1)[iso_country1 %in% missing_dispensable_countries]
      vcat(2," - toolCountryFill set missing values for DISPENSABLE countries to ",fill,":")
      vcat(2,paste(" --- ",names_countries,paste0("(",iso_country1[names_countries],")"),sep=""),sep="")
    }
    
    tmp <- new.magpie(missing_countries,getYears(x),getNames(x),fill=fill)
    x <- mbind(x,tmp)
    
    #map data as defined by additional mappings
    if(!is.null(map)) {
      for(i in 1:length(map)) {
        x[names(map)[i],,] <- setCells(x[map[i],,],names(map)[i])
      }
      vcat(2," - toolCountryFill set missing values to values of existing countries:")
      vcat(2,paste(" --- ",paste(map,names(map),sep=" -> "),sep=""),sep="")      
    }
    
  }
  
  #order regions by region name
  x <- x[sort(getRegions(x)),,]
  
  return(updateMetadata(x,calcHistory="update"))  
}
