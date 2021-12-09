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
#' @param threshold Population threshold in million capita which determines whether
#' the country is put into the "important" or "dispensable" class
#' (default = 1 mio. people)
#' @return vector of default ISO country codes.
#' @note Please always use this function instead of directly referring to the data
#' object as the format in this data list might change in the future!
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getSources}}, \code{\link{getCalculations}}
#' @examples
#'
#' head(getISOlist())
#' head(getISOlist("dispensable"))
#' @importFrom magclass ncells
#' @export
getISOlist <- function(type = "all", threshold = 1) {
  isoCountry <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
  isoCountry1 <- as.vector(isoCountry[, "x"])
  names(isoCountry1) <- isoCountry[, "X"]
  ref <- robustSort(isoCountry1)
  if (type == "all") return(ref)

  pop2015 <- readRDS(system.file("extdata", "pop2015.rds", package = "madrat"))
  names(dimnames(pop2015)) <- c("ISO2_WB_CODE", "Year", "data")

  if (type == "important") {
    return(ref[as.vector(pop2015[, 1, 1] >= threshold)])
  } else if (type == "dispensable") {
    return(ref[as.vector(pop2015[, 1, 1] < threshold)])
  } else {
    stop("Unknown type ", type)
  }
}
