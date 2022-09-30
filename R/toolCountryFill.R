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
#' @param countrylist character vector of official country names (if other than ISO)
#' @param ... Mappings between countries for which the data is missing and
#' countries from which the data should be used instead for these countries
#' (e.g. "HKG"="CHN" if Hong Kong should receive the value of China). This
#' replacement usually only makes sense for intensive values. Can be also provided
#' as a argument called "map" which contains a named vector of these mappings.
#' @return A MAgPIE object with spatial entries for each country of the
#' official ISO code country list.
#' @author Jan Philipp Dietrich
#' @examples
#'
#' library(magclass)
#' x <- new.magpie("DEU", 1994, "bla", 0)
#' y <- toolCountryFill(x, 99)
#' @importFrom magclass getItems new.magpie getYears getNames mbind setCells
#' @export
toolCountryFill <- function(x, fill = NA, no_remove_warning = NULL, overwrite = FALSE, verbosity = 1, # nolint
                            countrylist = NULL, ...) {
  comment <- getComment(x)
  if (is.null(countrylist)) {
    isoCountry <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
    countrylist <- as.vector(isoCountry[, "x"])
    names(countrylist) <- isoCountry[, "X"]
  }
  missingCountries    <- setdiff(countrylist, getItems(x, dim = 1.1))
  additionalCountries <- setdiff(getItems(x, dim = 1.1), countrylist)

  # remove irrelevant information
  if (length(additionalCountries) > 0) {
    x <- x[setdiff(getItems(x, dim = 1.1), additionalCountries), , ]
    # warn only for countries which were not explicitly mentioned in argument "remove"
    countries2warn <- setdiff(additionalCountries, no_remove_warning)
    if (length(countries2warn) > 0) {
      historicalCountries <- unique(read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"),
                                              stringsAsFactors = FALSE)[["fromISO"]])
      removedHistoricalCountries <- intersect(countries2warn, historicalCountries)
      historicalHint <- if (length(removedHistoricalCountries) > 0) {
        paste(" - By using madrat::toolISOhistorical the data for the following countries can usually still be used:",
              paste(removedHistoricalCountries, collapse = ", "))
      } else {
        ""
      }
      warning("Data for following unknown country codes removed: ", paste(countries2warn, collapse = ", "),
              historicalHint)
    }
  }

  # check mappings
  map <- c(...)
  if (!is.null(map)) {
    if (!is.null(names(map))) names(map) <- sub("^.*\\.", "", names(map))
    if (any(map %in% missingCountries)) {
      tmp <- map[map %in% missingCountries]
      stop("Try to fill a country with data from another country which is non-existent in the data set (",
        paste(tmp, names(tmp), sep = " -> ", collapse = " | "), ").")
    }
    if (!all(names(map) %in% missingCountries)) {
      if (overwrite) {
        tmp <- map[!(names(map) %in% missingCountries)]
        vcat(1, "Existing data overwritten with data from another country (",
          paste(tmp, names(tmp), sep = " -> ", collapse = " | "), ").")
      } else {
        map <- map[(names(map) %in% missingCountries)]
        if (length(map) == 0) map <- NULL
      }
    }
  }

  # add missing data
  if (length(missingCountries) > 0) {
    if (length(fill) <= 1) {
      fillMessage <- fill
    } else {
      fillMessage <- "a given vector of values"
    }
    missingImportantCountries <- setdiff(intersect(missingCountries, getISOlist("important")), names(map))
    if (length(missingImportantCountries) > 0) {
      namesCountries <- names(countrylist)[countrylist %in% missingImportantCountries]
      vcat(verbosity, " - toolCountryFill set missing values for IMPORTANT countries to ", fillMessage, ":")
      vcat(verbosity, " --- ", paste0(namesCountries, " (", countrylist[namesCountries], ") "), show_prefix = FALSE)
    }

    missingDispensableCountries <- setdiff(intersect(missingCountries, getISOlist("dispensable")), names(map))
    if (length(missingImportantCountries) > 0) {
      namesCountries <- names(countrylist)[countrylist %in% missingDispensableCountries]
      vcat(2, " - toolCountryFill set missing values for DISPENSABLE countries to ", fillMessage, ":")
      vcat(2, " --- ", paste0(namesCountries, " (", countrylist[namesCountries], ") "), show_prefix = FALSE)
    }

    tmp <- new.magpie(missingCountries, getYears(x), getNames(x), fill = fill)
    if (length(x) == 0) {
      x <- tmp
    } else {
      x <- mbind(x, tmp)
    }

    # map data as defined by additional mappings
    if (!is.null(map)) {
      for (i in seq_along(map)) {
        x[names(map)[i], , ] <- setCells(x[map[i], , ], names(map)[i])
      }
      vcat(2, " - toolCountryFill set missing values to values of existing countries:")
      vcat(2, " --- ", paste(map, names(map), sep = " -> "), show_prefix = FALSE)
    }

  }

  # order regions by region name
  x <- x[robustSort(getItems(x, dim = 1.1)), , ]
  getComment(x) <- comment
  return(x)
}
