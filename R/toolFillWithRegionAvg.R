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
#' (as returned by toolMappingFile) if no mapping is specified.
#' @param verbose Boolean variable indicating if the function should print out what it is doing.
#' Can generate a lot of output for a large object.
#' @param warningThreshold If more than this fraction of the countries in a given region and
#' timestep have a missing value, throw a warning.
#' @return A MAgPIE object with the missing values filled.
#' @author Bjoern Soergel, Lavinia Baumstark
#' 
#' @importFrom magclass as.magpie is.magpie getRegions getYears dimSums
#' @importFrom assertthat assert_that
#' @importFrom rlang is_empty
#' @export
#' 
#' @examples
#' 
#' x <- magclass::new.magpie(cells_and_regions = c('A','B','C','D'), years = c(2000,2005), 
#' fill = c(1,NA,3,4,5,6,NA,8))
#' rel <- data.frame(CountryCode = c('A','B','C','D'), RegionCode = c('R1','R1','R1','R2'))
#' xfilled <- toolFillWithRegionAvg(x, regionmapping = rel)


toolFillWithRegionAvg <- function(x, valueToReplace = NA, weight = NULL, callToolCountryFill = FALSE, 
                                  regionmapping = NULL, verbose = TRUE, warningThreshold = 0.5){

  assert_that(is.magpie(x))
  # limit to one data dimension at a time (avoids potential pitfalls with weight dimensions)
  assert_that(ndata(x) == 1)
  
  # if no weights are specified use unweighted average
  if (is.null(weight)){
    weight <- as.magpie(x)
    weight[] <- 1.
  } else {
    assert_that(ndata(weight) == 1)
    # ensure that all countries have weights
    assert_that(is_empty(setdiff(getRegions(x),getRegions(weight))))
    assert_that(is_empty(setdiff(getYears(x),getYears(weight))))
    weight <- weight[getRegions(x),getYears(x),]
  }
  
  # fill missing countries
  if (callToolCountryFill){
    x <- toolCountryFill(x, fill = valueToReplace)
  }
  
  # set values to be replaced to NA if not already the case
  if (!is.na(valueToReplace)){
    x[x == valueToReplace] <- NA
  }
  
  # get default mapping if no mapping is defined
  if (is.null(regionmapping)){
    map <- read.csv(toolMappingFile("regional",getConfig("regionmapping")),sep=";")
  } else {
    map <- regionmapping
  }
  
  # container for new values
  x_new <- as.magpie(x)
  
  # computation of regional averages and replacing
  for (regi in unique(map$RegionCode)){
    c_regi <- map$CountryCode[map$RegionCode==regi]
    for (yr in getYears(x)){
      # filter out the countries that are NA
      NAvals <- is.na(x[c_regi,yr,])
      # if no NAs -> jump to next iteration
      if (sum(NAvals) == 0) next
      c_NA <- c_regi[NAvals]
      c_vals <- c_regi[!NAvals]
      
      # weighted aggregation. convert to numeric to avoid issue with single country avg
      fill_val <- as.numeric(dimSums(x[c_vals,yr,]*weight[c_vals,yr,], dim = 1)/dimSums(weight[c_vals,yr,], dim = 1))
      x_new[c_NA,yr,] <- fill_val
      
      if (verbose) {
        print(sprintf("%s %s : replaced %s missing values with regional average of %.2e", 
                      regi,yr,length(c_NA),fill_val))
      }
      
      if (length(c_NA)/length(c_regi) > warningThreshold) {
        warning(sprintf("%s %s : more than %s percent missing values \n",regi,yr,100*warningThreshold))
      } 
      
    }
  }
  
  return(updateMetadata(x_new,x))
}