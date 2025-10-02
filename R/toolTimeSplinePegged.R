#' Smooth a magclass time series with optional anchor years
#'
#' `toolTimeSplinePegged()` applies the same smoothing spline approach as
#' `toolTimeSpline()`, but lets callers specify years that should be more heavily
#' weighted during spline fitting ("pegged" years). Anchoring is enforced by
#' applying high weights to those years, which strongly encourages (but does not
#' guarantee) that the spline will stay close to the original values.
#'
#' @param x A magclass object.
#' @param dof Degrees-of-freedom per 100 years (higher -> more degrees of freedom, less smoothing; default 5).
#' @param peggedYears Integer vector of years (e.g. `c(2020, 2050, 2100)`) to hold fixed; NULL for none.
#' @param anchorFactor Numeric multiplier for anchor weights (default 10); larger values more strongly enforce pegging.
#'
#' @return A magclass object of the same shape, with each time series spline-smoothed.
#' @author Michael Crawford
#' @importFrom stats smooth.spline
#' @export

toolTimeSplinePegged <- function(x,
                                 dof = 5,
                                 peggedYears = NULL, # NULL is no anchoring (old behavior)
                                 anchorFactor = 10) {

  ## 1) Input checks
  if (!is.magpie(x)) {
    warning("Input must be a magclass (MAgPIE) object!")
  }

  ## 2) Time axis & df calculation
  years <- getYears(x, as.integer = TRUE)
  nyr <- length(years)
  if (nyr < 2) {
    message("Less than two time steps: nothing to smooth.")
    return(x)
  }
  timespan <- years[nyr] - years[1]

  if (!is.numeric(dof) || dof < 1) {
    stop("dof must be a positive numeric value.")
  }
  if (dof > 30) {
    warning("High dof vs. timespan may reduce smoothing effect.")
  }
  dfValue <- timespan * dof / 100

  ## 3) Build weight vector
  if (is.null(peggedYears)) {
    # old behavior: no anchors
    wts <- rep(1, nyr)
    peggedYearsAll <- NULL
  } else {
    # parse user‐supplied anchors (allow "yYYYY" or numeric)
    peggedYearsAll <- as.integer(sub("^y", "", as.character(peggedYears), ignore.case = TRUE))
    # keep only years present in data
    peggedYearsAll <- intersect(peggedYearsAll, years)
    if (!all(peggedYearsAll %in% years)) {
      stop("One or more user-supplied anchors not in data years.")
    }

    wts <- rep(1, nyr)
    wts[years %in% peggedYearsAll] <- nyr * anchorFactor
  }

  ## 4) Per-series spline (uses fit$y so no predict() call)
  tmpspline <- function(ts, df) {
    fit <- stats::smooth.spline(
      x            = years,
      y            = ts,
      w            = wts,
      df           = df,
      control.spar = list(high = 2)
    )
    fit$y
  }

  ## 6) Apply over time-series (dim 2 inner)
  arrIn <- as.array(x)
  arrOut <- apply(arrIn, c(1, 3), tmpspline, df = dfValue)

  ## 7) Reconstruct magpie object
  dimnames(arrOut)[[1]] <- getYears(x)
  names(dimnames(arrOut))[1] <- getSets(x, fulldim = FALSE)[2]
  out <- as.magpie(arrOut, spatial = 2, temporal = 1)

  ## 8) Comment and return
  anchorText <- if (is.null(peggedYearsAll)) {
    "none"
  } else {
    paste(peggedYearsAll, collapse = ",")
  }
  comment <- paste0(
    getComment(x),
    "; toolTimeSplinePegged smoothed (anchors: ", anchorText,
    "; df=", round(dfValue, 2), ") [", date(), "]"
  )
  getComment(out) <- comment

  return(out)
}
