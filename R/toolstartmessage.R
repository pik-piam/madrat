#' Tool: Start message
#'
#' This function writes a process start message (what function was called with which arguments) and stores the current
#' time, so the corresponding call to \code{\link{toolendmessage}} can calculate the elapsed time.
#'
#' @param functionCallString A string representing the function call that should be logged
#' @param level This argument allows to establish a hierarchy of print statements. The hierarchy is preserved for the
#' next vcat executions. Currently this setting can have 4 states: NULL (nothing will be changed), 0 (reset
#' hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease hierarchy level by 1).
#' @return A list containing diagnostic information required by \code{\link{toolendmessage}}.
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{toolendmessage}}, \code{\link{vcat}}
#' @importFrom utils str
#' @examples
#'
#' innerFunction <- function() {
#'   startinfo <- madrat:::toolstartmessage("innerFunction(argumentsToPrint = 123)", "+")
#'   vcat(1, "inner")
#'   madrat:::toolendmessage(startinfo, "-")
#' }
#' outerFunction <- function() {
#'   startinfo <- madrat:::toolstartmessage("outerFunction()", "+")
#'   vcat(1, "outer")
#'   innerFunction()
#'   madrat:::toolendmessage(startinfo, "-")
#' }
#' outerFunction()
toolstartmessage <- function(functionCallString, level = NULL) {
  setWrapperInactive("wrapperChecks")

  vcat(1, "Run ", functionCallString, level = level, fill = 300, show_prefix = FALSE)
  return(list(time1 = proc.time(), functionCallString = functionCallString))
}
