#' redirectSource
#'
#' redirectSource will call a source specific redirect function if it exists
#' (called e.g. redirectTau), in which case the arguments are passed on to that
#' function. If such a function is not available \code{\link{redirect}} is called.
#' @param type The source dataset type. Passed on to the specific redirect
#' function or \code{\link{redirect}}.
#' @param target The target folder or files. Passed on to the specific redirect
#' function or \code{\link{redirect}}.
#' @param ... Additional arguments, passed on to the specific redirect function.
#' @param linkOthers Passed on to the specific redirect function or \code{\link{redirect}}.
#' @param local Passed on to the specific redirect function or \code{\link{redirect}}.
#' @return The result of the specific redirect function or \code{\link{redirect}}.
#' @author Pascal Sauer
#' @export
redirectSource <- function(type, target, ..., linkOthers = TRUE, local = TRUE) {
  if (is.environment(local)) {
    localEnvir <- local
  } else if (local) {
    localEnvir <- parent.frame()
  } else {
    localEnvir <- globalenv()
  }

  specificRedirect <- get0(paste0("redirect", type), mode = "function")
  if (is.null(specificRedirect)) {
    if (...length() > 0) {
      warning("redirectSource calls madrat::redirect, so additional arguments are ignored.")
    }
    return(redirect(type = type, target = target, linkOthers = linkOthers, local = localEnvir))
  } else {
    return(specificRedirect(target = target, ..., linkOthers = linkOthers, local = localEnvir))
  }
}
