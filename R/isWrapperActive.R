#' isWrapperActive
#'
#' Support function which checks whether a given wrapper function is currently in-use
#' or not.
#'
#' @param name name of the wrapper in question (e.g. "calcOutput")
#' @author Jan Philipp Dietrich
isWrapperActive <- function(name) {
  wrapperActive <- .readWrapperStatus()
  if(!(name %in% names(wrapperActive))) stop("Unknown wrapper \"", name, "\"!")
  return(wrapperActive[[name]])
}

#' @describeIn setWrapperActive set wrapper activity status
setWrapperActive <- function(name, value = TRUE) {
  if(!is.logical(value)) stop("Value must be a boolean!")
  wrapperActive <- .readWrapperStatus()
  if(!(name %in% names(wrapperActive))) stop("Unknown wrapper \"", name, "\"!")
  wrapperActive[[name]] <- value
  local_options(madrat_wrapperActive = wrapperActive, .local_envir = parent.frame())
}

.readWrapperStatus <- function() {
  wrapper <- c("downloadSource", "readSource", "calcOutput", "retrieveData", "wrapper")
  wrapperActive <- getOption("madrat_wrapperActive")
  if(is.null(wrapperActive)) {
    wrapperActive <- as.list(rep(FALSE, length(wrapper)))
    names(wrapperActive) <- wrapper
  }
  return(wrapperActive)
}
