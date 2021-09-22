#' toolCountry2isocode
#' 
#' Function used to convert country names from the long name to the ISO 3166-1
#' alpha 3 country code
#' 
#' 
#' @param country A vector of country names
#' @param warn whether warnings should be printed now or in the end of the
#' whole process as notes
#' @param type deprecated and will be removed soon!
#' @param mapping additional mappings as a names vector
#' @return the ISO 3166-1 alpha 3 country code
#' @author Jan Philipp Dietrich, Anastasis Giannousakis
#' @seealso \code{\link{readSource}},\code{\link{getSources}}
#' @examples
#' 
#' toolCountry2isocode("Germany")
#' toolCountry2isocode(c("Germany","Fantasyland"),mapping=c("Fantasyland"="BLA"))
#' @export

toolCountry2isocode <- function(country, warn = TRUE, type = NULL, mapping = NULL) {
  if (!is.null(type)) warning("\"type\" argument is deprecated and will be ignored! Don't use it!")
  country2iso <- read.csv2(system.file("extdata","country2iso.csv", package = "madrat"), 
                           row.names = NULL, encoding = "UTF-8")
  country2iso1 <- as.vector(country2iso[,"x"])
  names(country2iso1) <- country2iso[,"X"]
  if (is.null(mapping)) {
    mapping <- country2iso1
  } else {
    mapping <- c(country2iso1,mapping)
  }
  names(mapping) <- tolower(names(mapping))
  country <- as.factor(country)

  tmp <- as.character(mapping[tolower(levels(country))])
  if (warn && any(is.na(tmp))) warning("Following country names could not be found in the country list and returned as NA: ", 
                                       paste(levels(country)[is.na(tmp)],collapse = ", "))

  levels(country) <- tmp
  return(as.character(country))
}
