#' toolTimeAverage
#'
#' average over time given an averaging range
#' 
#' @param x magclass object that should be averaged
#' @param averaging_range number of time steps to average (if annual=FALSE, please check consistency)
#' @param cut if TRUE, all time steps at the start and end that can not be averaged correctly, will be removed 
#'            if FALSE, time steps at the start and end will be averaged with high weights for start and end points
#' @param annual if TRUE checks for completeness of annual coverage in the data set
#'               if FALSE no checks are performed
#'
#' @return the averaged data in magclass format
#' @author Kristine Karstens
#'
#' @export

toolTimeAverage <- function(x, averaging_range=NULL, cut=TRUE, annual=TRUE){
  
  if (!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  
  if (is.null(averaging_range)) averaging_range <- 1
  if (averaging_range < 1) {
    warning("Invalid choice of averaging_range. Value ", averaging_range, " is not allowed! Value is set to 1 instead!")
    averaging_range <- 1
  }
  #in the case of an even number of years, that should be used for averaging, the average is not symmetric to the corresponding year
  #in this case one year more is taken in the past then in the future of the correpsonding year
  averaging_steps <- -floor(averaging_range/2) + (0:(averaging_range - 1))
  years           <- getItems(x, dim = 2)
  
  # check average_range < length(years)
  if (averaging_range > length(years)) stop("Averaging range is greater than number of time steps.")
  
  # check for missing years, if annual option is true
  complete_years <- as.numeric(gsub("y","",years[1])):as.numeric(gsub("y","",years[length(years)]))
  if (annual & !setequal(complete_years, as.numeric(gsub("y","", years)))) stop("Not all time steps between start and end year are present.")
  if (!annual) warning("Time set completeness is not checked.")
  
  # Calculate weight matrix for using toolAggregate to average over time
  mat             <- array(0,dim = c(length(years), length(years)))
  rownames(mat)   <- colnames(mat)   <- years
  mat[(col(mat) - row(mat)) %in% averaging_steps] <- 1

  if (cut == FALSE) {
  # set weights at start and end points higher counts to offset missing years
  # (behaves as if start/end values would be constant before/after start/end)
    for (i in rownames(mat[averaging_range - rowSums(mat) != 0,])) {
      if (match(i,years) < length(years)/2) mat[i,1] <- averaging_range + 1 - sum(mat[i,])
      else mat[i,length(years)]  <- averaging_range + 1 - sum(mat[i,])
    }
  }
    
  out <- toolAggregate(x, rel = mat, dim = 2)/averaging_range
  out <- out[,rowSums(mat) == averaging_range,]
  
  getComment(out) <- c(getComment(x), paste0("Data averaged (toolTimeAverage): ",date()))
  return(out)
}