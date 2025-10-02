#' Smooth a magclass time series with optional anchor years
#'
#' `toolTimeSplinePegged()` applies the same smoothing spline approach as
#' `toolTimeSpline()`, but lets callers specify years that must remain equal to
#' their original values ("pegged" years).  Anchoring is enforced by heavily
#' weighting those years during spline fitting *and* copying the untouched
#' original observations back into the result after smoothing so that the anchor
#' years are guaranteed to match exactly.
#'
#' @param x   A magclass object.
#' @param dof Degrees-of-freedom per 100 years (higher → more degrees of freedom, less smoothing; default 5).
#' @param peggedYears Integer vector of years (e.g. `c(2020, 2050, 2100)`) to hold fixed; NULL for none.
#' @param anchorFactor Numeric multiplier for anchor weights (default 10); larger values more strongly enforce pegging.
#'
#' @return A magclass object of the same shape, with each time series spline-smoothed.
#' @author Michael Crawford
#' @importFrom stats smooth.spline predict
#' @export

toolTimeSplinePegged <- function(x,
                                 dof = NULL,
                                 peggedYears = NULL, # NULL is no anchoring (old behavior)
                                 anchorFactor = 10) {
  ## 1) Input checks
  if (!is.magpie(x)) {
    stop("Input must be a magclass (MAgPIE) object!")
  }

  ## 2) Time axis & df calculation
  years <- getYears(x, as.integer = TRUE)
  nyr <- length(years)
  if (nyr < 2) {
    warning("Less than two time steps: nothing to smooth.")
    return(x)
  }
  timespan <- years[nyr] - years[1]

  if (is.null(dof)) {
    dofParam <- 5
  } else if (!is.numeric(dof) || dof < 1) {
    warning("Invalid dof; resetting to 5.")
    dofParam <- 5
  } else {
    dofParam <- dof
  }
  if (dofParam > 30) {
    warning("High dof vs. timespan may reduce smoothing effect.")
  }
  dfValue <- timespan * dofParam / 100

  ## 3) Build weight vector
  if (is.null(peggedYears)) {
    # old behavior: no anchors
    wts <- rep(1, nyr)
    peggedYearsAll <- NULL
  } else {
    # parse user‐supplied anchors (allow "yYYYY" or numeric)
    yrsUser <- as.integer(sub("^y", "", as.character(peggedYears), ignore.case = TRUE))
    if (!requireNamespace("magpiesets", quietly = TRUE)) {
      warning("magpiesets not available; historical anchoring skipped.")
      histYrs <- integer(0)
    } else {
      pastStr <- magpiesets::findset("past") # e.g. "y1965",…
      histYrs <- as.integer(sub("^y", "", pastStr, ignore.case = TRUE))
    }
    # combine user anchors + historical period + endpoints
    peggedYearsAll <- unique(c(years[1], years[nyr], histYrs, yrsUser))
    # keep only years present in data
    peggedYearsAll <- intersect(peggedYearsAll, years)
    if (!all(yrsUser %in% years)) {
      stop("One or more user-supplied anchors not in data years.")
    }

    wts <- rep(1, nyr)
    wts[years %in% peggedYearsAll] <- nyr * anchorFactor
  }

  ## 4) Preserve non-negativity flag
  negativeFlag <- any(as.array(x) < 0)

  ## 5) Per-series spline (uses fit$y so no predict() call)
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

  ## 6) Apply over time-series (dim 2 inner) – same as original
  arrIn <- as.array(x)
  arrOut <- apply(arrIn, c(1, 3), tmpspline, df = dfValue)

  ## 7) Reconstruct magpie object
  dimnames(arrOut)[[1]] <- as.character(years)
  names(dimnames(arrOut))[1] <- getSets(x, fulldim = FALSE)[2]
  out <- as.magpie(arrOut, spatial = 2, temporal = 1)

  if (!is.null(peggedYearsAll)) {
    ## 7a) For anchored years, reinsert the original data to guarantee the
    ## spline has not nudged the values despite the high weights.
    yrInt <- getYears(out, as.integer = TRUE)
    pegIdx <- yrInt %in% peggedYearsAll
    if (any(pegIdx)) {
      yrLab <- getYears(out)[pegIdx]
      out[, yrLab, ] <- x[, yrLab, ]
    }
  }

  ## 8) Enforce non-negativity
  if (!negativeFlag) out[out < 0] <- 0

  ## 9) Comment and return
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
