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
#'
#' @return A magclass object of the same shape, with each time series spline-smoothed.
#' @author Kristine Karstens, Felicitas Beier, Michael Crawford
#' @family temporal processing
#' @importFrom stats smooth.spline
#' @export

toolTimeSpline <- function(x,
                           dof = 5,
                           peggedYears = NULL,
                           anchorFactor = 10) {

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

  ## 3) Build weight vector
  if (is.null(peggedYears)) {
    # no anchors
    weights <- rep(1, nyr)
    peggedYearsAll <- NULL
  } else {
    # parse user‐supplied anchors (allow "yYYYY" or numeric)
    peggedYearsAll <- as.integer(sub("^y", "", as.character(peggedYears), ignore.case = TRUE))
    # keep only years present in data
    peggedYearsAll <- intersect(peggedYearsAll, years)
    if (!all(peggedYearsAll %in% years)) {
      stop("One or more user-supplied anchors not in data years.")
    }

    weights <- rep(1, nyr)
    weights[years %in% peggedYearsAll] <- nyr * anchorFactor
  }

  ## 4) Per-series spline (uses fit$y so no predict() call)
  tmpspline <- function(ts, df) {
    fit <- stats::smooth.spline(
      x            = years,
      y            = ts,
      w            = weights,
      df           = df,
      control.spar = list(high = 2)
    )
    fit$y
  }

  ## 5) Apply over time-series (dim 2 inner)
  arrIn <- as.array(x)
  d     <- dim(arrIn)
  nseries <- d[1] * d[3]

  # smooth.spline() requires 1 < df <= n (#unique x); outside that range it
  # silently ignores df and picks a df via GCV instead (with a warning), which
  # makes the fit data-dependent and breaks the linearity argument below. Only
  # take the fast path when dfValue is valid, so smooth.spline's own choice of
  # df is guaranteed identical (and therefore linear) across all series.
  validDf <- dfValue > 1 && dfValue <= nyr

  if (nseries >= nyr && validDf) {
    # smooth.spline with fixed x, w and df is a *linear* smoother: for any
    # input y, fitted = smoothMat %*% y, where smoothMat depends only on
    # (years, wts, df) and not on the data itself. Build smoothMat once from
    # the nyr unit vectors (via the unchanged tmpspline() above, so it stays
    # in sync with any edge-case behaviour of smooth.spline), then replace
    # nseries individual smooth.spline() calls with per-band matrix
    # multiplications. This is algebraically identical to the direct
    # per-series apply() below and only pays off once there are at least as
    # many series as spline evaluations required to build smoothMat.
    #
    # Note: deliberately *not* done via a single aperm()'d (nyr x nseries)
    # matrix -- on objects with tens of thousands of cells that aperm (plus
    # the one needed to transpose back) each materialise a full extra copy of
    # x, multiplying peak memory well past what apply() below needs. Looping
    # per band and right-multiplying keeps the cell axis untouched, so the
    # only extra full-size buffer is the (already unavoidable) output array.
    if (anyNA(arrIn)) {
      stop("toolTimeSpline: x contains NA values, which smooth.spline cannot handle.")
    }
    smoothMat <- vapply(seq_len(nyr), function(i) {
      unit <- numeric(nyr)
      unit[i] <- 1
      tmpspline(unit, dfValue)
    }, numeric(nyr))
    smoothMatT <- t(smoothMat)

    arrOut <- array(NA_real_, dim = d, dimnames = dimnames(arrIn))
    for (b in seq_len(d[3])) {
      # matrix() (rather than relying on [ , , b]'s default drop behaviour)
      # keeps this correct when d[1] == 1
      cellMat <- matrix(arrIn[, , b], nrow = d[1], ncol = nyr)
      arrOut[, , b] <- cellMat %*% smoothMatT
    }
  } else {
    arrOut <- apply(arrIn, c(1, 3), tmpspline, df = dfValue)
    dimnames(arrOut)[[1]] <- dimnames(arrIn)[[2]]
    arrOut <- aperm(arrOut, c(2, 1, 3))
  }

  ## 6) Reconstruct magpie object
  dimnames(arrOut)[[2]] <- getYears(x)
  names(dimnames(arrOut))[2] <- getSets(x, fulldim = FALSE)[2]
  out <- as.magpie(arrOut, spatial = 1, temporal = 2)

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
