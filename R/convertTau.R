#' Convert Tau
#'
#' Convert landuse intensity data (tau) to data on ISO country level.
#'
#'
#' @param x MAgPIE object containing tau values and corresponding weights xref
#' at 0.5deg cellular level.
#' @return Tau data and weights as MAgPIE object aggregated to country level
#' @author Jan Philipp Dietrich
#' @importFrom magclass ncells getCells<- collapseNames getCells

convertTau <- function(x) {

  "!# @monitor madrat:::sysdata$iso_cell magclass:::ncells"
  "!# @ignore  madrat:::toolAggregate"

  tau  <- x[, , "tau"]
  xref <- x[, , "xref"]

  # clean data
  # make sure that the weight for nonexisting tau values is 10^-10
  xref[is.na(tau)] <- 10^-10
  # fill gaps within tau factors with 1 (the global mean)
  tau[is.na(tau)] <- 1

  # calculate numbers on country level if they are provided on cellular level
  if (ncells(x) == 59199) {
    # read mapping cells -> iso countries
    isoCell <- sysdata$iso_cell
    isoCell[, 2] <- getCells(x)

    # aggregate data
    tau  <- toolAggregate(tau,  rel = isoCell, weight = collapseNames(xref))
    xref <- toolAggregate(xref, rel = isoCell)
  }

  # check whether the country list agrees with the list of countries in the madrat package
  # remove unrequired data, add missing data
  tau  <- toolCountryFill(tau, fill = 1, TLS = "IDN", HKG = "CHN", SGP = "CHN", BHR = "QAT")
  xref <- toolCountryFill(xref, fill = 0, verbosity = 2)

  return(mbind(tau, xref))
}
