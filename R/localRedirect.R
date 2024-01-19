#' @export
localRedirect <- function(type, target) {
  if (!is.null(target)) {
    target <- normalizePath(target, mustWork = TRUE)
  }

  redirections <- getConfig("redirections")
  redirections[[type]] <- target
  setConfig(redirections = redirections, .local = parent.frame())
  return(redirections) # TODO invisible
}
