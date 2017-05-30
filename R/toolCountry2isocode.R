#' toolCountry2isocode
#' 
#' Function used to convert country names from the long name to the ISO 3166-1
#' alpha 3 country code
#' 
#' 
#' @param country A vector of country names
#' @param warn whether warnings should be printed now or in the end of the
#' whole process as notes
#' @param type the name of the source type
#' @param mapping additional mappings as a names vector
#' @return the ISO 3166-1 alpha 3 country code
#' @author Jan Philipp Dietrich, Anastasis Giannousakis
#' @seealso \code{\link{readSource}},\code{\link{getSources}}
#' @examples
#' 
#' toolCountry2isocode("Germany")
#' toolCountry2isocode(c("Germany","Fantasyland"),mapping=c("Fantasyland"="BLA"))
#' @export

toolCountry2isocode <- function(country,warn=TRUE,type="IEA",mapping=NULL) {
  country2iso <- read.csv2(system.file("extdata","country2iso.csv",package = "madrat"),row.names=NULL,encoding="UTF-8")
  country2iso1<-as.vector(country2iso[,"x"])
  names(country2iso1)<-country2iso[,"X"]
  if(is.null(mapping)) {
    mapping <- country2iso1
  } else {
    mapping <- c(country2iso1,mapping)
  }
  names(mapping) <- tolower(names(mapping))
  country <- tolower(country)
  
  if (length(country) > 100) { # transformation into factor for better performance. Applied only to large objects.
    country<-as.factor(country)
    tmp<-as.character(mapping[levels(country)])
    if(any(is.na(tmp)) & warn==TRUE) {
      warning("Following country names could not be found in the country list and returned as NA: ", paste(levels(country)[is.na(tmp)],collapse=", "))
    } else if(any(is.na(tmp)) & warn==FALSE) {
      b<-getOption("madrat_cfg")
      note<-paste0("Note: In the toolCountry2isocode call, following country names could not be found in the country list and returned as NA: ", paste(levels(country)[is.na(tmp)],collapse=", "))
      b$sources[[type]]<-c(b$sources[[type]],notes=note)
      options(madrat_cfg = b)
    }
    levels(country)<-tmp
    return(as.character(country))
    } else {
    tmp <- as.character(mapping[country])
    if(any(is.na(tmp)) & warn==TRUE) {
      warning("Following country names could not be found in the country list and returned as NA: ", paste(levels(country)[is.na(tmp)],collapse=", "))
    } else if(any(is.na(tmp)) & warn==FALSE) {
      b<-getOption("madrat_cfg")
      note<-paste0("Note: In the toolCountry2isocode call, following country names could not be found in the country list and returned as NA: ", paste(levels(country)[is.na(tmp)],collapse=", "))
      b$sources[[type]]<-c(b$sources[[type]],notes=note)
      options(madrat_cfg = b)
    }    
    return(tmp)
  }
}
