#' installedMadratUniverse
#'
#' Returns a name vector of installed packages which supposedly belong to the
#' madrat universe.   They are currently derived as the union of
#' - all packages registered under `getConfig("packages")`,
#' - all packages with a name starting with "mr" or "ms" (as the usual
#'   indicator for madrat-packages and madrat-support-packages), and
#' - all packages having `madrat` as either a `Depends` or `Imports`
#'   dependency.
#'
#' @md
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
  pkgs <- installed.packages()
  robustSort(unique(c(
                      getConfig("packages"),
                      grep("^m[rs]", rownames(pkgs), value = TRUE),
                      rownames(pkgs)[grepl("\\bmadrat\\b", pkgs[, "Depends"])
                                     | grepl("\\bmadrat\\b",
                                             pkgs[, "Imports"])])))
}
