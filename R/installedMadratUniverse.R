#' installedMadratUniverse
#'
#' Returns a name vector of installed packages which
#' supposedly belong to the madrat universe.
#' They are currently derived as the union of
#' all loaded madrat packages and all packages
#' with a name starting with "mr" or "ms"
#' (as the usual indicator for madrat-packages and
#' madrat-support-packages).
#'
#' @return A name vector of installed packages which supposedly belong
#' to the madrat universe
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}
#' @examples
#' \dontrun{
#' installedMadratUniverse()
#' }
#' @export
#'
installedMadratUniverse <- function() {
 return(robustSort(union(getConfig("packages"), grep("^m[rs]", rownames(installed.packages()), value = TRUE))))
}
