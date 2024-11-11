#' toolTimeAverage
#'
#' average over time given an averaging range. Only works for data with
#' equidistant time steps!
#'
#' @param x magclass object that should be averaged with equidistant time steps
#' @param averaging_range number of time steps to average
#' @param cut if TRUE, all time steps at the start and end that can not be averaged correctly, will be removed
#'            if FALSE, time steps at the start and end will be averaged with high weights for start and end points
#' @return the averaged data in magclass format
#'
#' @author Kristine Karstens, Jan Philipp Dietrich
#' @export
toolTimeAverage <- function(x, averaging_range = NULL, cut = TRUE) { # nolint
  if (!is.magpie(x)) {
    stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  }
  averagingRange <- averaging_range

  if (is.null(averagingRange)) {
    averagingRange <- 1
  }
  if (averagingRange < 1) {
    warning("Replacing invalid averaging_range ", averagingRange, " with 1.")
    averagingRange <- 1
  }
  # in the case of an even number of time steps, that should be used for averaging, the average is not symmetric
  # to the corresponding year. In this case one time step more is taken in the past then in the future of
  # the corresponding year
  averagingSteps <- -floor(averagingRange / 2) + (0:(averagingRange - 1))
  years           <- getItems(x, dim = 2)

  # check average_range < length(years)
  if (averagingRange > length(years)) {
    stop("Averaging range is greater than number of time steps.")
  }

  # check for equidistant years
  y <- getYears(x, as.integer = TRUE)
  timeStepLength <- unique(utils::tail(y, length(y) - 1) - utils::head(y, length(y) - 1))
  if (length(timeStepLength) != 1) {
    stop("toolTimeAverage requires equidistant years (yearDiff in data: ",
         paste(timeStepLength, collapse = ", "), ")")
  }

  # Calculate weight matrix for using toolAggregate to average over time
  mat             <- array(0, dim = c(length(years), length(years)))
  rownames(mat)   <- colnames(mat)   <- years
  mat[(col(mat) - row(mat)) %in% averagingSteps] <- 1

  if (!cut) {
    # set weights at start and end points higher counts to offset missing years
    # (behaves as if start/end values would be constant before/after start/end)
    for (i in rownames(mat[averagingRange - rowSums(mat) != 0, ])) {
      if (match(i, years) < length(years) / 2) {
        mat[i, 1] <- averagingRange + 1 - sum(mat[i, ])
      } else {
        mat[i, length(years)]  <- averagingRange + 1 - sum(mat[i, ])
      }
    }
  }

  out <- toolAggregate(x, rel = mat, dim = 2) / averagingRange
  out <- out[, rowSums(mat) == averagingRange, ]

  getComment(out) <- c(getComment(x), paste0("Data averaged (toolTimeAverage): ", date()))
  return(out)
}
