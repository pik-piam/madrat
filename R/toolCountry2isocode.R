#' toolCountry2isocode
#'
#' Function used to convert country names from the long name to the ISO 3166-1
#' alpha 3 country code
#'
#'
#' @param country A vector of country names
#' @param warn whether warnings should be printed now or in the end of the
#' whole process as notes
#' @param ignoreCountries A vector of country names/codes that exist in the
#' data and that should be removed but without creating a
#' warning (they will be removed in any case). You should use that argument if
#' you are certain that the given entries should be actually removed from the
#' data.
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
toolCountry2isocode <- function(country, warn = TRUE, ignoreCountries = NULL, type = NULL, mapping = NULL) {
  if (!is.null(type)) warning("\"type\" argument is deprecated and will be ignored! Don't use it!")
  country2iso <- read.csv2(system.file("extdata", "country2iso.csv", package = "madrat"),
                           row.names = NULL, encoding = "UTF-8")
  country2iso1 <- as.vector(country2iso[, "x"])
  names(country2iso1) <- country2iso[, "X"]
  mapping <- c(country2iso1, mapping)
  names(mapping) <- tolower(names(mapping))
  country <- as.factor(country)

  tmp <- as.character(mapping[tolower(levels(country))])
  if (warn && any(is.na(tmp))) {
    countries2warn <- setdiff(levels(country)[is.na(tmp)], ignoreCountries)
    if (length(countries2warn) > 0) {
      warning("Following country names could not be found in the country list and returned as NA: ",
              paste0('"', countries2warn, '"', collapse = ", "))
    }
  }
  levels(country) <- tmp
  return(as.character(country))
}
