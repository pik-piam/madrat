#' isWrapperActive
#'
#' Support functions which checks whether a given wrapper function is currently in-use
#' or not or which locally activate or deactivate a wrapper (setting will be
#' automatically resetted when a function finishes).
#'
#' @param name name of the wrapper in question (e.g. "calcOutput")
#' @author Jan Philipp Dietrich
isWrapperActive <- function(name) {
  wrapperActive <- .readWrapperStatus(name)
  return(wrapperActive[[name]])
}

#' @describeIn isWrapperActive set wrapper activity status to on
setWrapperActive <- function(name) {
  wrapperActive <- .readWrapperStatus(name)
  wrapperActive[[name]] <- TRUE
  local_options(madrat_wrapperActive = wrapperActive, .local_envir = parent.frame())
}

#' @describeIn isWrapperActive set wrapper activity status to off
setWrapperInactive <- function(name) {
  wrapperActive <- .readWrapperStatus(name)
  wrapperActive[[name]] <- FALSE
  local_options(madrat_wrapperActive = wrapperActive, .local_envir = parent.frame())
}

.readWrapperStatus <- function(name = NULL) {
  wrapper <- list(downloadSource = FALSE,
                      readSource = FALSE,
                      calcOutput = FALSE,
                    retrieveData = FALSE,
                   wrapperChecks = TRUE)
  wrapperActive <- getOption("madrat_wrapperActive")
  if (is.null(wrapperActive)) {
    wrapperActive <- wrapper
  } else {
    for (n in names(wrapper)) {
      if (!is.logical(wrapperActive[[n]])) wrapperActive[[n]] <- wrapper[[n]]
    }
  }
  if (!is.null(name) && !(name %in% names(wrapperActive))) stop("Unknown wrapper \"", name, "\"!")
  return(wrapperActive)
}
