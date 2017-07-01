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
  vcat(1,"Run",functioncall,level=level)
  startdata <- list(time1=proc.time())
  d <- getConfig("diagnostics")
  if(is.character(d)) {
    filename <- paste0(getConfig("outputfolder"),"/",d,".csv")
    if(!file.exists(filename)) {
      x <- data.frame(functioncall=functioncall,start=TRUE,time=as.numeric(startdata$time1["elapsed"]),runtime=-1,id="none")
    } else {
      x <- read.table(filename,stringsAsFactors = FALSE, sep = ";", header = TRUE, quote = "")
      x <- rbind(x,c(functioncall,TRUE,startdata$time1["elapsed"],-1,"none"))
    }
    try(write.table(x,filename,row.names = FALSE, quote = FALSE, sep=";"))
  }
  return(startdata)
}