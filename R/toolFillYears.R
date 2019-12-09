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

toolFillYears<-function(x, years){

  if(class(years)=="character") years <- as.numeric(substring(years, 2))
  out                        <- new.magpie(getCells(x), years, getNames(x))

  # Fill in x contained years to out, if requested by argument years
  contained_years            <- getYears(x, as.integer = TRUE)
  contained_years            <- intersect(contained_years, years)

  out[,contained_years,]     <- x[,contained_years,]

  # Interpolate years between contained years linear, if requested by argument years
  interpolate_years          <- seq(contained_years[1], contained_years[length(contained_years)], 1)
  interpolate_years          <- intersect(interpolate_years, years)

  out[,interpolate_years,]   <- time_interpolate(x, interpolate_years, integrate_interpolated_years=TRUE)[,interpolate_years,]

  # Hold constant before and after contained years, if requested by argument years
  missingyears               <- setdiff(years, interpolate_years)
  beforeyears                <- missingyears[missingyears < contained_years[1]]
  afteryears                 <- missingyears[missingyears > contained_years[1]]
  out[,beforeyears,]         <- x[,contained_years[1],]
  out[,afteryears,]          <- x[,contained_years[length(contained_years)],]

  return(out)
}
