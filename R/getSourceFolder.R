#' getSourceFolder
#'
#' Return the path to source data files for the given type and subtype. This
#' applies redirections, see \code{\link{redirectSource}} for more details.
#'
#' @param type Dataset name, e.g. "Tau" for \code{\link{readTau}}
#' @param subtype Subtype of the dataset, e.g. "paper" for \code{\link{readTau}}, NULL is allowed
#' @return Path to source data files
#' @author Pascal Sauer
getSourceFolder <- function(type, subtype) {
  redirections <- getConfig("redirections")
  if (type %in% names(redirections)) {
    sourcefolder <- normalizePath(redirections[[type]], mustWork = TRUE)
  } else {
    sourcefolder <- file.path(getConfig("sourcefolder"), make.names(type))
  }

  if (!is.null(subtype) && file.exists(file.path(sourcefolder, make.names(subtype), "DOWNLOAD.yml"))) {
    sourcefolder <- file.path(sourcefolder, make.names(subtype))
  }

  return(sourcefolder)
}
