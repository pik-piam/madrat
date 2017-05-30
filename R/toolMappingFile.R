#' Tool: MappingFile
#' 
#' Functions which calculates the path to a mapping file
#' 
#' 
#' @param type Mapping type ("regional", or "sectoral")
#' @param name File name of the mapping file.
#' @param readcsv if true, file read in
#' @param error.missing Boolean which decides whether an error is returned if
#' the mapping file does not exist or not.
#' @return The complete path to the mapping file.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{toolMappingFile("sectoral","structuremappingFE.csv")}
#' 
#' 
#' @export
#' 
toolMappingFile <- function(type,name,readcsv=FALSE,error.missing=TRUE) {
  mf <- getConfig("mappingfolder")
  if(is.null(mf)) stop('No mappingfolder specified in used cfg! Please load a config with the corresponding information!')
  if(!file.exists(mf)) stop('The mappings folder "', mf, '" does not exist!')
  if(!file.exists(paste0(mf,"/",type))) stop('Unknown mappings type "',type,'"!')
  fname <- paste0(mf,"/",type,"/",name)
  if(error.missing) if(!file.exists(fname)) stop('Mapping "',name,'" not found!')
  if(readcsv){
    if(grepl(pattern = ";",x=readLines(fname,1))){
      out <- read.csv(fname,sep = ";",colClasses = "character")
    } else {
      out <- read.csv(fname,sep = ",",colClasses = "character")
    }
  } else {
    out <- fname
  }
  return(out)
}
