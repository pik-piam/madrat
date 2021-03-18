#' Tool: End message
#' 
#' Function writes a process end message and performs some diagnostics
#' 
#' 
#' @param startdata a list containing diagnostic information provided by \code{\link{toolstartmessage}}
#' @param level This argument allows to establish a hierarchy of print
#' statements. The hierarchy is preserved for the next vcat executions.
#' Currently this setting can have 4 states: NULL (nothing will be changed), 0
#' (reset hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
#' hierarchy level by 1).
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{toolstartmessage}}, \code{\link{vcat}}
#' @examples
#' 
#' \dontrun{
#' tmp <- function(bla=NULL) {
#'   startinfo <- toolstartmessage("+")
#'   print(bla)
#'   toolendmessage(startinfo,"-")
#'   }
#' tmp(bla=99)
#' }
#'

toolendmessage <- function(startdata, level=NULL) {
  startdata$time2 <- proc.time()
  runtime <- round((startdata$time2 - startdata$time1)["elapsed"], 2)
  functioncall <-  paste(deparse(sys.call(-1)), collapse = "")
  vcat(1, "Exit ", functioncall, " in ", runtime, " seconds", level = level, 
       fill = 300, show_prefix = FALSE)
}