#' cleanComment
#'
#' Helper function to clean a comment from additional metadata information
#'
#' @md
#' @param x magclass object the comment should be read from
#' @param remove Vector of categories to be removed
#' @author Jan Philipp Dietrich
#' @examples
#' x <- maxample("animal")
#' getComment(x) <- c("unit: bla", "comment: hallo", "blub: ble")
#' madrat:::cleanComment(x)


cleanComment <- function(x, remove = c("unit", "description", "comment", "origin", "creation date", "note")) {
  # remove old descriptors
  x <- getComment(x)
  out <- grep(paste0("^ *(", paste(remove, collapse = "|"), "):"), x, value = TRUE, invert = TRUE)
  if (length(out) == 0) return(NULL)
  return(out)
}
