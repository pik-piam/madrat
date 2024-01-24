#' redirectSource
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
#' @param .local The scope of the redirection, passed on to setConfig. Defaults to the current function.
#' Set to an environment for more control or to FALSE for a permanent/global redirection.
#' @return Invisibly, a list of all redirections where names are types and
#' values are the paths these types are redirected to.
#' @author Pascal Sauer
#' @examples \dontrun{
#' f <- function() {
#'   redirectSource("Tau", target = "~/TauExperiment")
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
redirectSource <- function(type, subtype = NULL, ..., target, .local = TRUE) {
  # Redirecting only specific subtypes is not supported to avoid tricky cases
  # where the subtype is ignored (search for "getSourceFolder\(.*subtype = NULL\)").
  stopifnot(is.null(subtype))

  # TODO call source-specific inject function if it exists

  if (is.environment(.local)) {
    .localEnvir <- .local
  } else if (.local) {
    .localEnvir <- parent.frame()
  } else {
    .localEnvir <- globalenv()
  }

  if (!is.null(target)) {
    target <- normalizePath(target, mustWork = TRUE)
    if (length(target) >= 2 || !dir.exists(target)) {
      tempDir <- withr::local_tempdir(.local_envir = .localEnvir)
      file.symlink(target, file.path(tempDir, basename(target)))
      target <- tempDir
    }
  }

  redirections <- getConfig("redirections")
  redirections[[type]] <- target
  setConfig(redirections = redirections, .local = .localEnvir)
  return(invisible(redirections))
}
