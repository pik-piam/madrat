#' wrappedReadSource
#'
#' Minimal wrapper for readSource.
#' Beware that no checks are done that the input follows the required naming conventions.
#'
#' @param sourceObject A list with the following elements:
#' \itemize{
#'   \item read A function to read the data. Mandatory.
#'   \item correct A function to correct the data. Optional.
#'   \item covert A function to convert the data (to country level). Optional.
#'   \item download A function to download the data. Optional.
#' }
#' @inheritParams readSource
#' @inherit readSource return
#'
#' @author Pascal Sauer
#'
#' @export
wrappedReadSource <- function(sourceObject, subtype = NULL, subset = NULL) {
  stopifnot(is.function(sourceObject$read))
  return(wrappedReadSourceFunctions(sourceObject$read,
                                    sourceObject$correct,
                                    sourceObject$convert,
                                    sourceObject$download,
                                    subtype,
                                    subset))
}
