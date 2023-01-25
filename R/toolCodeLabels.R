#' Tool: CodeLabels
#'
#' This function replaces a hash code (e.g. regioncode) or another cryptic
#' code with a human readable code via a given dictionary.
#' This can be useful to make outputs better readable in cases where hash
#' codes are already known to the user.
#' If no entry exists in the dictionary the hash code is returned again.
#'
#' @param get A vector of hash codes which should be replaced
#' @param add Additional entries that should be added to the dictionary. Need to be
#' provided in the form of a named vector with the structure c(<label>=<hash>),
#' e.g. c(h12="62eff8f7")
#' @return A vector with either labels (if available) or hash codes (if no label was available).
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{regionscode}}
#' @examples
#' toolCodeLabels("62eff8f7")
#'
#' @export
toolCodeLabels <- function(get = NULL, add = NULL) {
  labels <- getOption("madrat_codelabels")
  if (is.null(labels)) add <- c(h12 = "62eff8f7", add)
  if (!is.null(add) && is.vector(add) && !is.null(names(add))) {
    labels[add] <- names(add)
    options(madrat_codelabels = labels) # nolint
  }
  if (!is.null(get)) {
    out <- as.vector(labels[get])
    out[is.na(out)] <- get[is.na(out)]
    return(out)
  }
}
