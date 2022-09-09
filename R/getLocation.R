#' getLocation
#'
#' Returns names of packages in which functions matching the description
#' are found
#'
#'
#' @param name name of the function to be found. Can be either the full name
#' (e.g. "calcTauTotal"), or just the type name (e.g. "TauTotal").
#' @param packages A character vector with packages in which should be looked
#' for the function
#' @param globalenv Boolean deciding whether functions in the global environment
#' should be included or not
#' @return vector of packages in which a function matching the description
#' could be found
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCalculations}}, \code{\link{getDependencies}}
#' @export

getLocation <- function(name, packages = installedMadratUniverse(), globalenv = TRUE) {
  all <- getCalculations("read|calc|full", packages = packages, globalenv = globalenv)
  all$name <- sub("^[^:]*\\:{3}", "", all$call)
  name <- tolower(name)
  all$type <- tolower(all$type)
  all$name <- tolower(all$name)
  out <- unique(all$package[name == all$type | name == all$name])
  if (length(out) == 0) out <- NULL
  return(out)
}
