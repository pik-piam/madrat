#' redirectTau
#'
#' redirectTau will be called by redirectSource when type = "Tau". Redirects
#' the Tau source folder to the target folder.
#'
#' @param target The target folder or files.
#' @param ... Passed on to \code{\link{redirect}}.
#'
#' @examples
#' \dontrun{
#' redirectSource("Tau", "a/different/tau-source-folder")
#' a <- readSource("Tau", "paper")
#' }
#' @author Pascal Sauer
redirectTau <- function(target, ...) {
  # could do something source specific here, like extracting a tgz archive
  redirect("Tau", target = target, ...)
}
