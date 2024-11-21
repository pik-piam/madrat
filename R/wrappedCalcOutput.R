#' wrappedCalcOutput
#'
#' Minimal wrapper for calcOutput.
#' Beware that no checks are done that the input follows the required naming conventions.
#'
#' @md
#' @param calc A function to calculate the data. Mandatory. Function name must start with 'calc'. The
#' rest of the function name determines the 'type'.
#' @param aggregate Boolean indicating whether output data aggregation should be performed or
#' not, "GLO" (or "glo") for aggregation to one global region,
#' "REG+GLO" (or "regglo") for a combination of regional and global data.
#' @param file A file name. If given the output is written to that file in the
#' outputfolder as specified in the config.
#' @param years A vector of years that should be returned. If set to NULL all
#' available years are returned.
#' @param round Number of decimal places to round to.  Ignored if `NULL`.  See
#'   [`round()`] for details.
#' @param signif Number of significant digits to round to.  Ignored if `NULL`.
#'   See [`signif()`] for details.
#' @param supplementary boolean deciding whether supplementary information such as weight should be
#' returned or not. If set to TRUE a list of elements will be returned!
#' @param append boolean deciding whether the output data should be appended in the existing file.
#' Works only when a file name is given in the function call.
#' @param warnNA boolean deciding whether NAs in the data set should create a warning or not
#' @param na_warning deprecated, please use \code{warnNA} instead
#' @param try if set to TRUE the calculation will only be tried and the script will continue even if
#' the underlying calculation failed. If set to FALSE calculation will stop with an error in such a
#' case. This setting will be overwritten by the global setting debug=TRUE, in which try will be
#' always interpreted as TRUE.
#' @param regionmapping alternative regionmapping to use for the given calculation. It will temporarily
#' overwrite the global setting just for this calculation.
#' @param writeArgs a list of additional, named arguments to be supplied to
#' the corresponding write function
#' @param ... Additional settings directly forwarded to the corresponding
#' calculation function
#' @export
wrappedCalcOutput <- function(
    calc, aggregate = TRUE, file = NULL, years = NULL, round = NULL, signif = NULL,
    supplementary = FALSE, append = FALSE, warnNA = TRUE, na_warning = NULL, try = FALSE,
    regionmapping = NULL, writeArgs = NULL, ...) {
                       
    type <- sub("^.*?calc", "", deparse(substitute(calc)))
    calcOutput(
        type, aggregate=aggregate, file=file, years=years, round=round, signif=signif,
        supplementary=supplementary, append=append, warnNA=warnNA, na_warning=na_warning, try=try,
        regionmapping=regionmapping, writeArgs=writeArgs, ...
    )
    }
