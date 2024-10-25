#' prepExtendedComment
#'
#' Helper function condense metadata information into
#' an extended comment entry
#'
#' @md
#' @param x list containing the metadata to be condensed
#' @param type output type, e.g. "TauTotal"
#' @param warn boolean indicating whether warnings should be triggered
#' if entries are missing, or not.
#' @param n the number of generations to go back
#' @author Jan Philipp Dietrich
#' @examples
#' test <- function(a = 1) {
#'    return(madrat:::prepExtendedComment(list(unit = "m", description = "example", package = "blub")))
#'  }
#'  test(a = 42)
#'
prepExtendedComment <- function(x, type = "#undefined", warn = TRUE, n = 1) {

  cl <- sys.call(-n)
  f <- get(as.character(cl[[1]]), mode = "function", sys.frame(-n - 1))
  cl <- match.call(definition = f, call = cl)

  if (isTRUE(warn)) {
    unitWarning <- paste0('Missing unit information for data set "', type, '"!')
    descriptionWarning <- paste0('Missing description for data set "', type,
                                 '"! Please add a description in the corresponding calc function!')
  } else {
    unitWarning <- descriptionWarning <- NULL
  }

  unit        <- prepComment(x$unit, "unit", unitWarning)
  description <- prepComment(x$description, "description", descriptionWarning)
  comment     <- prepComment(cleanComment(x$x), "comment")
  origin      <- prepComment(paste0(gsub("\\s{2,}", " ", paste(deparse(cl), collapse = "")),
                                    " (madrat ", unname(getNamespaceVersion("madrat")), " | ", x$package, ")"),
                             "origin")
  date        <- prepComment(date(), "creation date")
  note        <- prepComment(x$note, "note")

  extendedComment <- c(description,
                       unit,
                       note,
                       comment,
                       origin,
                       date)
  return(extendedComment)
}
