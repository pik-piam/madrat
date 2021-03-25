#' Tool: Start message
#' 
#' Function writes a process start message and performs some diagnostics
#' 
#' 
#' @param level This argument allows to establish a hierarchy of print
#' statements. The hierarchy is preserved for the next vcat executions.
#' Currently this setting can have 4 states: NULL (nothing will be changed), 0
#' (reset hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
#' hierarchy level by 1).
#' @return a list containing diagnostic information required by \code{\link{toolendmessage}}
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{toolendmessage}}, \code{\link{vcat}}
#' @importFrom utils read.table
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

toolstartmessage <- function(level=NULL) {
  functioncall <- paste(deparse(sys.call(-1)),collapse="")
  vcat(-2,"Run ",functioncall, level=level, fill=300)
  startdata <- list(time1=proc.time())
  return(startdata)
}
