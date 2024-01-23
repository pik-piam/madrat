
#' localRedirect
#'
#' Redirect a given dataset type to a different source folder. The redirection
#' is local, so it will be reset when the current function call returns. See
#' example for more details.
#'
#' @param type Dataset name, e.g. "Tau" for \code{\link{readTau}}
#' @param subtype Not supported for now, must be NULL. This is here to simplify
#' a potential later implementation of subtype redirections.
#' @param ... Additional arguments, passed on to source-specific inject function if it exists
#' @param target Path to the new source folder, NULL to remove the redirection
#' @return Invisibly, a list of all redirections where names are types and
#' values are the paths these types are redirected to.
#' @author Pascal Sauer
#' @examples \dontrun{
#' f <- function() {
#'   localRedirect("Tau", target = "~/TauExperiment")
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
localRedirect <- function(type, subtype = NULL, ..., target) {
  # Redirecting only specific subtypes is not supported to avoid tricky cases
  # where the subtype is ignored (search for "getSourceFolder\(.*subtype = NULL\)").

  # TODO call source-specific inject function if it exists

  # TODO allow target to be paths to files instead of single folder
  if (!is.null(target)) {
    target <- normalizePath(target, mustWork = TRUE)
  }

  redirections <- getConfig("redirections")
  redirections[[type]] <- target
  # TODO allow other scopes, e.g. global or parent.frame(2)
  setConfig(redirections = redirections, .local = parent.frame())
  return(invisible(redirections))
}
