#' prepComment
#'
#' Helper function to properly format a metadata comment entry
#'
#' @md
#' @param x content to be added as metadata comment
#' @param name Name of the metadata entry
#' @param warning Either NULL (no warning) or a warning text that should be
#' returned if x is NULL
#' @author Jan Philipp Dietrich
#' @examples
#'  madrat:::prepComment("example comment", "example")

prepComment <- function(x, name, warning = NULL) {
  if (!is.null(x)) {
    x[1] <- paste0(" ", name, ": ", x[1])
    if (length(x) > 1) {
      x[2:length(x)] <- paste0(paste(rep(" ", 3 + nchar(name)), collapse = ""), x[2:length(x)])
    }
  } else {
    if (!is.null(warning)) {
      vcat(0, warning)
      x <- paste0(" ", name, ": not provided")
    }
  }
  return(x)
}
