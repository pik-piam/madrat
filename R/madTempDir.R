#' madTempDir
#'
#' returns a temporary directory as a subfolder of the tempfolder set in
#' \code{getConfig("tmpfolder")}.
#' @return path to the temp folder
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getConfig}}
#' @examples
#' \dontrun{
#' madrat:::madTempDir()
#' }
#'
madTempDir <- function() {
  d <- file.path(getConfig("tmpfolder"), basename(tempdir()))
  if (!dir.exists(d)) dir.create(d)
  return(d)
}
