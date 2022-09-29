#' @title toolFillYears
#' @description Inter- and extrapolates a historical dataset for a given time period.
#'
#' @param x MAgPIE object to be continued.
#' @param years vector of years as digits or in mag year format
#' @return MAgPIE object with completed time dimensionality.
#' @author Kristine Karstens
#'
#' @importFrom magclass time_interpolate
#' @export

toolFillYears <- function(x, years) {

  if (inherits(years, "character")) years <- as.numeric(substring(years, 2))
  out                        <- new.magpie(getCells(x), years, getNames(x), sets = getSets(x))

  # Fill in x contained years to out, if requested by argument years
  containedYears            <- getYears(x, as.integer = TRUE)
  containedYears            <- intersect(containedYears, years)

  out[, containedYears, ]   <- x[, containedYears, ]

  # Interpolate years between contained years linear, if requested by argument years
  interpolateYears          <- seq(containedYears[1], containedYears[length(containedYears)], 1)
  interpolateYears          <- intersect(interpolateYears, years)

  out[, interpolateYears, ] <- time_interpolate(x, interpolateYears,
                                                  integrate_interpolated_years = TRUE)[, interpolateYears, ]

  # Hold constant before and after contained years, if requested by argument years
  missingyears               <- setdiff(years, interpolateYears)
  beforeyears                <- missingyears[missingyears < containedYears[1]]
  afteryears                 <- missingyears[missingyears > containedYears[1]]
  out[, beforeyears, ]       <- x[, containedYears[1], ]
  out[, afteryears, ]        <- x[, containedYears[length(containedYears)], ]

  return(out)
}
