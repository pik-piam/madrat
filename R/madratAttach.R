#' madratAttach / madratDetach
#'
#' Attaches the madrat functions of a package to the currently active madrat
#' universe or detaches it again from it.
#'
#' @param package name of the package to be loaded. Alternative, the path to
#' the package.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getConfig}}, \code{\link{setConfig}}
#' @examples
#' \dontrun{
#' madratAttach("madrat")
#' }
#' @export
madratAttach <- function(package) {
  setConfig(packages = c(getConfig("packages"), basename(package)),
            .cfgchecks = FALSE, .verbose = FALSE)
}

#' @describeIn madratAttach detach package from madrat universe
#' @export
madratDetach <- function(package) {
  setConfig(packages = setdiff(getConfig("packages"), basename(package)),
            .cfgchecks = FALSE, .verbose = FALSE)
}
