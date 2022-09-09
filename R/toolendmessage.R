#' Tool: End message
#'
#' This function writes a process end message and performs some diagnostics. It is always called after a corresponding
#' call to \code{\link{toolstartmessage}}.
#'
#' @param startdata a list containing diagnostic information provided by \code{\link{toolstartmessage}}
#' @param level This argument allows to establish a hierarchy of print
#' statements. The hierarchy is preserved for the next vcat executions.
#' Currently this setting can have 4 states: NULL (nothing will be changed), 0
#' (reset hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
#' hierarchy level by 1).
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{toolstartmessage}}, \code{\link{vcat}}
toolendmessage <- function(startdata, level = NULL) {
  runtime <- unname(round((proc.time() - startdata$time1)["elapsed"], 2))
  vcat(1, "Exit ", startdata$functionCallString, " in ", runtime, " seconds",
       level = level, fill = 300, show_prefix = FALSE)
}
