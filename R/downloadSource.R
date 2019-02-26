#' downloadSource
#' 
#' Download a source. The function is a wrapper for specific functions designed
#' for the different possible source types.
#' 
#' 
#' @param type source type, e.g. "IEA". A list of all available source types
#' can be retrieved with function \code{\link{getSources}("download")}.
#' @param overwrite Boolean deciding whether existing data should be
#' overwritten or not.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- downloadSource("Tau")
#' }
#' 
#' @export 
downloadSource <- function(type,overwrite=FALSE) {
  startinfo <- toolstartmessage("+")
  on.exit(toolendmessage(startinfo,"-"))
  
  # check type input
  if(!is.character(type)) stop("Invalid type (must be a character)!")
  if(length(type)!=1)     stop("Invalid type (must be a single character string)!")
  
  functionname <- prepFunctionName(type=type, prefix="download")

  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  if(!file.exists(getConfig("sourcefolder"))) dir.create(getConfig("sourcefolder"), recursive = TRUE)
  setwd(getConfig("sourcefolder"))
  if(file.exists(type)) {
    if(overwrite) {
      unlink(type,recursive = TRUE)
    } else {
      stop("Source folder for source \"",type,"\" does already exist! Delete folder or activate overwrite to proceed!")
    }
  }
  dir.create(type)
  setwd(type)
  eval(parse(text=functionname))
  
  origin <- paste0("origin: ", gsub("\\s{2,}"," ",paste(deparse(match.call()),collapse=""))," -> ",functionname," (madrat ",packageDescription("madrat")$Version," | ",attr(functionname,"pkgcomment"),")")
  date <- paste0("download date: ", date())
  
  writeLines(c(origin,date),"download_info.txt")
}
