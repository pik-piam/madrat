#' toolTimeSpline
#'
#' Smoothing a data set by replacing its values by its spline approximation using the given degrees of freedom.
#' 
#' @param x magclass object that should be smoothed via a spline approximation
#' @param dof degrees of freedom per 100 years (similiar to an average range), is a proxy for the smoothness of the 
#' spline (smaller values = smoother)
#' 
#' @return approximated data in magclass format
#' @author Kristine Karstens, Felicitas Beier
#'
#' @importFrom stats smooth.spline
#' @export

toolTimeSpline <- function(x, dof=NULL){
  
  if (!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  
  negative <- any(x < 0)
  
  years    <- getYears(x, as.integer = TRUE)
  timespan <- years[length(years)] - years[1]
  
  if (is.null(dof)) {
    dof <- 5
  } else if (dof < 1) {
    warning("dof values < 1 not allowed! Value is set to 5 instead!")
    dof <- 5
  }
  
  # check dofs << length(years)
  if (dof > 30) warning("Degrees of freedom too high compared to time span to have an smoothing effect!")

  dof <- timespan*dof/100
  
  # fill in data with spline approximations/interpolations
  tmpspline <- function(x, dof) return(smooth.spline(x, df = dof, control.spar = list(high = 2))$y)
  out <- apply(as.array(x), c(1,3), tmpspline , dof = dof)
  dimnames(out)[[1]] <- getYears(x)
  names(dimnames(out))[1] <- getSets(x, fulldim = FALSE)[2]
  
  out <- as.magpie(out, spatial = 2, temporal = 1)

  # Correct for negative values if needed
  if (!negative) out[out < 0] <- 0

  getComment(out) <- c(getComment(x), paste0("Data averaged (toolTimeSpline): ",date()))
  return(out)
}
