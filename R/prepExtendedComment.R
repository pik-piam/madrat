#' prepExtendedComment
#'
#' Helper function condense metadata information into
#' an extended comment entry
#'
#' @md
#' @param x list containing the metadata to be condensed
#' @param type output type, e.g. "TauTotal"
#' @param functionCallString A string representation of the function call
#' that created the object this comment is attached to
#' @param warn boolean indicating whether warnings should be triggered
#' if entries are missing, or not.
#' @author Jan Philipp Dietrich, Pascal Sauer
prepExtendedComment <- function(x, type, functionCallString, warn = TRUE) {
  if (isTRUE(warn)) {
    unitWarning <- paste0('Missing unit information for data set "', type, '"!')
    descriptionWarning <- paste0('Missing description for data set "', type,
                                 '"! Please add a description in the corresponding calc function!')
  } else {
    unitWarning <- NULL
    descriptionWarning <- NULL
  }

  unit        <- prepComment(x$unit, "unit", unitWarning)
  description <- prepComment(x$description, "description", descriptionWarning)
  comment     <- prepComment(cleanComment(x$x), "comment")
  origin      <- prepComment(paste0(gsub("\\s{2,}", " ", functionCallString),
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
