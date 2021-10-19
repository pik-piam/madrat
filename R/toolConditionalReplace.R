#' toolConditionalReplace
#'
#' Sets values (NA, negative, ..) to value \code{replaceby}
#' @param x magpie object
#' @param conditions vector of conditions for values, that should be removed e.g. "is.na()", "< 0" (order matters)
#' @param replaceby value which should be used instead (can be a vector of same length as conditions as well)
#'
#' @return return changed input data
#' @author Kristine Karstens
#'
#' @export

toolConditionalReplace <- function(x, conditions, replaceby = 0) {
  if (length(replaceby) != length(conditions)) {
    if (length(replaceby) == 1) {
      replaceby <- rep(replaceby, length(conditions))
    } else {
      stop("'replaceby' has to be of length 1 or the same length as 'conditions'")
    }
  }

  for (i in seq_along(conditions)) {
    if (grepl("\\(\\)", conditions[i])) {
      conditions[i] <- paste0(strsplit(conditions[i], "\\)"), "x)")
    } else {
      conditions[i] <- paste0("x", conditions[i])
    }
    xCheck <- eval(parse(text = conditions[i]))
    xCheck[is.na(xCheck)] <- FALSE

    if (any(xCheck)) {
      percent    <- sum(xCheck) / length(x) * 100
      verbosity  <- ifelse(percent > 1, 1, 2)
      vcat(verbosity = verbosity, paste(percent, "% of data points with", conditions[i], "set to", replaceby[i], "."))
      x[xCheck] <- replaceby[i]
    }
  }

  return(x)
}
