#' toolTimeSpline
#'
#' Creating a spline for a time series using given degrees of freedom
#' 
#' @param x magclass object that should be interpolated/approximated with a spline
#' @param dof degrees of freedom per 100 years (similiar to an average range), is a proxy for the smoothness of the spline
#' 
#' @return approximated data in magclass format
#' @author Kristine Karstens
#'
#' @importFrom stats smooth.spline
#' @export

toolTimeSpline <- function(x, dof=NULL){
  
  if(!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  
  negative <- any(x<0)
  
  if (withMetadata() && !is.null(getOption("calcHistory_verbosity")) && getOption("calcHistory_verbosity")>1) {
    if (object.size(sys.call()) < 5000 && as.character(sys.call())[1]=="toolTimeSpline")  calcHistory <- "update"
    #Special calcHistory handling necessary for do.call(x$aggregationFunction,x$aggregationArguments) from calcOutput
    else  calcHistory <- paste0("toolTimeSpline(x=unknown, dof=",dof,")")
  } else  calcHistory <- "copy"
  
  years    <- getYears(x, as.integer = TRUE)
  timespan <- years[length(years)] - years[1]
  
  if(is.null(dof)){
    dof <- timespan*5/100
  } else if(dof<1) {
    warning(paste("Invalid choice of dof. Value",dof,"is not allowed! Value is set to 5 dof per 100 years instead!"))
    dof <- timespan*5/100
  } else {
    dof <- timespan*dof/100
  }
  
  # check dofs << length(years)
  if(dof > timespan*30/100) warning("Choice of degrees of freedom will create rather interpolation than an approximation.")
  
  out      <- x
  
  x <- as.array(x)
  
  # fill in data with spline approximations/interpolations
  tmpspline <- function(x,dof) return(smooth.spline(x,df=dof, control.spar=list(high=2))$y)
  out <- apply(x, c(1,3) ,tmpspline , dof=dof)
  dimnames(out)[[1]] <- getYears(x)
  out <- as.magpie(out)
  
  # Correct for negative values if needed
  if(negative) out[out<0] <- 0

  getComment(out) <- c(getComment(x), paste0("Data averaged (toolTimeSpline): ",date()))
  return(updateMetadata(out,unit="copy",calcHistory=calcHistory))
}
