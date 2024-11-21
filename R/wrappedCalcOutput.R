#' wrappedCalcOutput
#'
#' Minimal wrapper for calcOutput.
#' Beware that no checks are done that the input follows the required naming conventions.
#'
#' @param calc A function to calculate the data. Mandatory. Function name must start with 'calc'.
#' @inheritParams calcOutput
#' @inherit calcOutput return
#'
#' @author Sally Dacie, Pascal Sauer
#' @export
wrappedCalcOutput <- function(
    calc, aggregate = TRUE, file = NULL, years = NULL, round = NULL, signif = NULL,
    supplementary = FALSE, append = FALSE, warnNA = TRUE,
    na_warning = NULL, # nolint: object_name_linter.
    try = FALSE, regionmapping = NULL, writeArgs = NULL, ...) {
  type <- sub("^.*?calc", "", deparse(substitute(calc)))
  calcOutput(type, aggregate = aggregate, file = file, years = years, round = round, signif = signif,
             supplementary = supplementary, append = append, warnNA = warnNA, na_warning = na_warning,
             try = try, regionmapping = regionmapping, writeArgs = writeArgs, ...)
}
