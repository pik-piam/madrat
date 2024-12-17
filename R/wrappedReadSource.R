#' wrappedReadSource
#'
#' Wrapper for readSource accepting a source object instead of string.
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
wrappedReadSource <- function(sourceObject, subtype = NULL, subset = NULL,
                              convert = TRUE, supplementary = FALSE) {
  stopifnot(is.function(sourceObject$read))
  read <- sourceObject$read
  envList <- as.list(environment(read))
  envList <- envList[startsWith(names(envList), "read")]
  functionName <- names(envList)[Position(function(f) identical(f, read), envList)]
  type <- sub("^read", "", functionName)

  # get default subtype
  if (is.null(subtype)) {
    subtype <- formals(read)[["subtype"]]
  }

  return(readSource(type = type, subtype = subtype, subset = subset,
                    convert = convert, supplementary = supplementary))
}
