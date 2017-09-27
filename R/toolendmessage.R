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
#' @param id additional id which uniquely identifies the process that just has been finished
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

toolendmessage <- function(startdata, level=NULL, id="none") {
  startdata$time2 <- proc.time()
  runtime <- (startdata$time2-startdata$time1)["elapsed"]
  functioncall <-  paste(deparse(sys.call(-1)),collapse="")
  vcat(1,"Exit",functioncall,"in",runtime,"seconds",level=level, fill=300)
  d <- getConfig("diagnostics")
  if(is.character(d)) {
    filename <- paste0(getConfig("outputfolder"),"/",d,".csv")
    if(!file.exists(filename)) {
      x <- data.frame(functioncall=functioncall,start=FALSE,time=as.numeric(startdata$time2["elapsed"]),runtime=runtime,id=id)
    } else {
      x <- read.table(filename,stringsAsFactors = FALSE, sep=";", header=TRUE, quote = "")
      x <- rbind(x,c(functioncall,FALSE,startdata$time2["elapsed"],runtime,id))
    }
    suppressWarnings(try(write.table(x,filename,row.names = FALSE,quote = FALSE, sep=";"), silent=TRUE))
  }
}