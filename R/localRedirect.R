
#' localRedirect
#'
#' Redirect a given dataset type to a different source folder. The redirection
#' is local, so it will be reset when the current function call returns. See
#' example for more details.
#'
#' @param type Dataset name, e.g. "Tau" for \code{\link{readTau}}
#' @param target Path to the new source folder, NULL to remove the redirection
#' @return Invisibly, a list of all redirections where names are types and
#' values are the paths these types are redirected to.
#' @author Pascal Sauer
#' @examples \dontrun{
#' f <- function() {
#'   localRedirect("Tau", "~/TauExperiment")
#'   # the following call will change directory
#'   # into ~/TauExperiment instead of <getConfig("sourcefolder")>/Tau
#'   readSource("Tau")
#' }
#' f()
#' # Tau is only redirected in the local environment of f,
#' # so it will use the usual source folder here
#' readSource("Tau")
#' }
#' @export
localRedirect <- function(type, target) {
  if (!is.null(target)) {
    target <- normalizePath(target, mustWork = TRUE)
  }

  redirections <- getConfig("redirections")
  redirections[[type]] <- target
  setConfig(redirections = redirections, .local = parent.frame())
  return(invisible(redirections))
}
