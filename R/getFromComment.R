# SPDX-FileCopyrightText: 2025 Potsdam Institute for Climate Impact Research (PIK)
# SPDX-License-Identifier: BSD-2-Clause

#' getFromComment
#'
#' Helper function extract a metadata comment
#'
#' @md
#' @param x object the metadata should be extracted from
#' @param name name of the metadata to be extracted (e.g. unit)
#' @author Jan Philipp Dietrich
#' @examples
#' x <- as.magpie(1)
#' getComment(x) <- c(" description: example description", " unit: kg")
#' getFromComment(x, "unit")
#' getFromComment(x, "description")
#' @export

getFromComment <- function(x, name) {
  comment <- attr(x, "comment")
  key <- paste0("^ ", name, ": ")
  entry <- grep(key, comment)
  if (length(entry) == 0) return(NULL)
  if (length(entry) > 1) stop("duplicate metadata entries found!")
  return(sub(key, "", comment[entry]))
}
