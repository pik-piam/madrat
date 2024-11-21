#' wrappedReadSource
#'
#' Minimal wrapper for readSource.
#' Beware that no checks are done that the input follows the required naming conventions.
#' 
#' @param read A function to read the data. Mandatory. Function name must start with 'read'. The
#' rest of the function name determines the 'type'.
#' @param correct A function to correct the data. Optional. Function name must start with 'correct'
#' and end with the type, same as for read.
#' @param covert A function to convert the data (putting it at country level). Optional. Function 
#' name must start with 'convert' and end with the type, same as for read.
#' @param subtype A character string. For some sources there are subtypes of the source, for these
#' sources the subtype can be specified with this argument. If a source does not
#' have subtypes, subtypes should not be set.
#' @param subset A character string. Similar to \code{subtype} a source can also have \code{subsets}.
#' A \code{subsets} can be used to only read part of the data. This can in particular make sense
#' for huge data sets where reading in the whole data set might be impractical or even infeasible.
#' @export
wrappedReadSource <- function(read, correct = NULL, convert = NULL, subtype = NULL, subset = NULL) {

  # get string/bool from function(s) for use in readSource
  .getConvert <- function(convert, correct) {
    if (!is.null(convert)) {do_convert <- TRUE}
    else if (!is.null(correct)) {do_convert <- "onlycorrect"}
    else {do_convert <- FALSE}
    return(do_convert)
  }

  # get default subtype argument if available, otherwise NULL
  if (is.null(subtype)) {subtype <- formals(eval(read))[["subtype"]]}

  # conversions
  type <- sub("^.*?read", "", deparse(substitute(read)))
  do_convert <- .getConvert(convert=convert, correct=correct)

  # call readSource with original interface
  readSource(type=type, subtype=subtype, subset=subset, convert=do_convert)
}
