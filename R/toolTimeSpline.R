#' Smooth a magclass time series with optional anchor years
#'
#' Smooths a magclass time series using spline approximation with the given degrees
#' of freedom. Optionally, specific years can be "pegged" (anchored) to stay close
#' to their original values during smoothing. Anchoring is enforced by applying
#' high weights to those years.
#'
#' @param x A magclass object.
#' @param dof Degrees-of-freedom per 100 years (higher -> more degrees of freedom,
#'   less smoothing; default 5).
#' @param peggedYears Integer vector of years (e.g. `c(2020, 2050, 2100)`) to
#'   anchor during smoothing; NULL for none (default).
#' @param anchorFactor Numeric multiplier for anchor weights (default 10);
#'   larger values more strongly enforce pegging.
#' @param targetYears Integer vector of additional years at which the fitted spline
#'   should be evaluated, enabling higher time resolution (default NULL, i.e. output
#'   years equal input years). Output years are the sorted union of input and target years.
#'
#' @return A magclass object with each time series spline-smoothed.
#'   If targetYears is not specified (NULL), the time dimension is unchanged. If targetYears is
#'   given, the time dimension covers the sorted union of the original and target years.
#' @author Kristine Karstens, Felicitas Beier, Michael Crawford
#' @importFrom stats smooth.spline
#' @export

toolTimeSpline <- function(x,
                           dof = 5,
                           peggedYears = NULL,
                           anchorFactor = 10,
                           targetYears = NULL) {

  ## 1) Input checks
  if (!is.magpie(x)) {
    stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  }

  negative <- any(x < 0)

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

  ## 2b) Determine output years
  if (!is.null(targetYears)) {
    targetYears <- as.integer(sub("^y", "", as.character(targetYears), ignore.case = TRUE))
    outputYears <- sort(union(years, targetYears))
  } else {
    outputYears <- years
  }

  ## 3) Build weight vector
  if (is.null(peggedYears)) {
    # no anchors
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

  ## 4) Per-series spline
  tmpspline <- function(ts, df) {
    fit <- stats::smooth.spline(
      x            = years,
      y            = ts,
      w            = wts,
      df           = df,
      control.spar = list(high = 2)
    )
    if (is.null(targetYears)) fit$y else predict(fit, x = outputYears)$y
  }

  ## 5) Apply over time-series (dim 2 inner)
  arrIn <- as.array(x)
  arrOut <- apply(arrIn, c(1, 3), tmpspline, df = dfValue)

  ## 6) Reconstruct magpie object
  dimnames(arrOut)[[1]] <- paste0("y", outputYears)
  names(dimnames(arrOut))[1] <- getSets(x, fulldim = FALSE)[2]
  out <- as.magpie(arrOut, spatial = 2, temporal = 1)

  # Correct for negative values if needed
  if (!negative) out[out < 0] <- 0

  ## 7) Comment and return
  anchorText <- if (is.null(peggedYearsAll)) {
    "none"
  } else {
    paste(peggedYearsAll, collapse = ",")
  }
  comment <- paste0(
    getComment(x),
    "; toolTimeSpline smoothed (anchors: ", anchorText,
    "; df=", round(dfValue, 2), ") [", date(), "]"
  )
  getComment(out) <- comment

  return(out)
}
