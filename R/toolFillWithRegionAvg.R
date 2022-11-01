#' Tool: FillWithRegionAvg
#'
#' This function fills missing values for countries with the (weighted) average
#' of the respective region. The average is computed separately for every
#' timestep. Currently only inputs with one data dimension are allowed as inputs.
#' (If the filling should be performed over multiple data dimensions, call this
#' function multiple times and bind the results together with magclass::mbind.)
#'
#' toolFillWithRegionAvg can be used in conjunction with toolCountryFill() to first
#' fill up the list of countries to the official ISO code country list, and then
#' fill values with the regional average (see callToolCountryFill Option).
#'
#'
#' @param x MAgPIE object with country codes in the first and time steps in the
#' second dimension.
#' @param valueToReplace value that denotes missing data. Defaults to NA.
#' @param weight MAgPIE object with weights for the weighted average. Must contain
#' at least all the countries and years present in x. If no weights are specified,
#' an unweighted average is performed.
#' @param callToolCountryFill Boolean variable indicating whether the list of countries
#' should first be filled to the official ISO code country list. Subsequently the
#' newly added and previously missing values are filled with the region average.
#' @param regionmapping Data frame containing the mapping between countries and regions.
#' Expects column names CountryCode and RegionCode. Uses the currently set mapping
#' if no mapping is specified.
#' @param verbose Boolean variable indicating if the function should print out what it is doing.
#' Can generate a lot of output for a large object.
#' @param warningThreshold If more than this fraction of the countries in a given region and
#' timestep have a missing value, throw a warning.
#' @param noteThreshold If more than this fraction of the countries in a given region and
#' timestep have a missing value, a note will be written.
#' @return A MAgPIE object with the missing values filled.
#' @author Bjoern Soergel, Lavinia Baumstark, Jan Philipp Dietrich
#'
#' @importFrom magclass as.magpie is.magpie getItems getYears dimSums
#' @export
#'
#' @examples
#'
#' x <- magclass::new.magpie(cells_and_regions = c("A", "B", "C", "D"), years = c(2000, 2005),
#'   fill = c(1, NA, 3, 4, 5, 6, NA, 8))
#' rel <- data.frame(CountryCode = c("A", "B", "C", "D"), RegionCode = c("R1", "R1", "R1", "R2"))
#' xfilled <- toolFillWithRegionAvg(x, regionmapping = rel)
toolFillWithRegionAvg <- function(x, valueToReplace = NA, weight = NULL, callToolCountryFill = FALSE, # nolint
                                  regionmapping = NULL, verbose = TRUE, warningThreshold = 0.5, noteThreshold = 1) {

  if (!is.magpie(x)) stop("Input x has to be a MAgPIE object!")
  # limit to one data dimension at a time (avoids potential pitfalls with weight dimensions)
  if (!(ndata(x) == 1)) stop("Only one element in data dimension allowed!")

  # if no weights are specified use unweighted average
  if (is.null(weight)) {
    weight <- as.magpie(x)
    weight[] <- 1.
  } else {
    if (ndata(weight) != 1) stop("Weight must have exactly one element in data dimension!")
    # ensure that all countries have weights
    if (length(setdiff(getItems(x, dim = 1.1), getItems(weight, dim = 1.1))) > 0) {
      stop("Regions in x and weight do not match!")
    }
    if (length(setdiff(getYears(x), getYears(weight))) > 0) {
      stop("Years in x and weight do not match!")
    }
    weight <- weight[getItems(x, dim = 1.1), getYears(x), ]
  }

  # fill missing countries
  if (callToolCountryFill) {
    x <- toolCountryFill(x, fill = valueToReplace)
  }

  # set values to be replaced to NA if not already the case
  if (!is.na(valueToReplace)) {
    x[x == valueToReplace] <- NA
  }

  # get default mapping if no mapping is defined
  if (is.null(regionmapping)) {
    map <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  } else {
    map <- regionmapping
  }

  # container for new values
  xNew <- as.magpie(x)

  aboveThreshold <- list(warning = list(), note = list())
  replace <- NULL

  # computation of regional averages and replacing
  for (regi in unique(map$RegionCode)) {
    cRegi <- map$CountryCode[map$RegionCode == regi]
    cRegi <- intersect(cRegi, getItems(x, dim = 1.1))
    if (length(cRegi) == 0) next
    for (yr in seq_len(nyears(x))) {
      # filter out the countries that are NA
      naVals <- is.na(x[cRegi, yr, ])
      # if no NAs -> jump to next iteration
      if (sum(naVals) == 0) next
      cNA <- cRegi[naVals]
      cVals <- cRegi[!naVals]

      # weighted aggregation. convert to numeric to avoid issue with single country avg
      fillVal <- as.numeric(dimSums(x[cVals, yr, ] * weight[cVals, yr, ], dim = 1) / dimSums(weight[cVals, yr, ],
                                                                                             dim = 1))
      if (length(fillVal) == 0) fillVal <- NA
      xNew[cNA, yr, ] <- fillVal

      replace <- c(replace, paste0(regi, "|", yr, " (", length(cNA), "x) -> ", round(fillVal, 2)))
      if (length(cNA) / length(cRegi) > warningThreshold) {
        aboveThreshold$warning[[regi]] <- c(aboveThreshold$warning[[regi]], yr)
      }
      if (length(cNA) / length(cRegi) > noteThreshold) {
        aboveThreshold$note[[regi]] <- c(aboveThreshold$note[[regi]], yr)
      }

    }
  }
  if (verbose) {
    message("Replaced missing values with regional average for: ", paste(replace, collapse = ", "))
  }
  if (length(aboveThreshold$warning) > 0) {
    warning("More than ", 100 * warningThreshold, "% missing values for: ",
            paste(names(aboveThreshold$warning), collapse = ", "))
  } else if (length(aboveThreshold$note) > 0) {
    message("More than ", 100 * noteThreshold, "% missing values for: ",
         paste(names(aboveThreshold$note), collapse = ", "))
  }

  return(xNew)
}
