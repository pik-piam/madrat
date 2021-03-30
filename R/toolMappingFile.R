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
#' @param where location to look for the mapping, either "mappingfolder" or
#' the name of a package which contains the mapping
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
toolMappingFile <- function(type, name, readcsv=FALSE, error.missing=TRUE, where="mappingfolder") {
  .Deprecated("toolGetMapping")
  if(where=="mappingfolder") {
    mf <- getConfig("mappingfolder")
    if(is.null(mf)) stop('No mappingfolder specified in used cfg! Please load a config with the corresponding information!')
    fname <- paste0(mf,"/",type,"/",name)
    if(!file.exists(fname) & file.exists(system.file("extdata", name, package = "madrat"))) {
      vcat(-2,"copy mapping ",name," from madrat package into mappings folder...")
      if(!file.exists(paste0(mf,"/",type))) dir.create(paste0(mf,"/",type), recursive = TRUE)
      file.copy(system.file("extdata", name, package = "madrat"), fname)
    }
    if(error.missing & !file.exists(fname)) {
      if(!file.exists(mf)) stop('The mappings folder "', mf, '" does not exist!')
      if(!file.exists(paste0(mf,"/",type))) stop('Unknown mappings type "',type,'"!')
      stop('Mapping "',name,'" not found!')
    }
  } else {
    fname <- system.file("extdata", paste0(type,"/",name), package=where)
    if(fname=="" & error.missing) stop('Mapping "',name,'" with type "',type,'" not found in package "',where,'"!')
  }
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
