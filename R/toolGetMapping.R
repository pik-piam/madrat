#' Tool: GetMapping
#' 
#' Function which retrieves a mapping file
#' 
#' 
#' @param name File name of the mapping file. Supported file types are currently csv (, or ; separated), rds 
#' and rda (which needs to have the data stored with the object name "data"!). Use code{\link{toolConvertMapping}}
#' to convert between both formats
#' @param type Mapping type (e.g. "regional", "cell", or "sectoral"). Can be set to NULL if file
#' is not stored in a type specific subfolder
#' @param where location to look for the mapping, either "mappingfolder", "local" (if the path is relative to your current
#' directory) or the name of a package which contains the mapping
#' @param error.missing Boolean which decides whether an error is returned if
#' the mapping file does not exist or not.
#' @param returnPathOnly If set to TRUE only the file path is returned
#' @param activecalc If set, this argument helps to define the first package within
#' which the mapping has to be sought for. This happens via finding in which
#' package the active calc function is located. 
#' @return the mapping as a data frame
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}, \code{\link{toolConvertMapping}}
#' @examples
#' 
#' head(toolGetMapping("regionmappingH12.csv", where="madrat"))
#' 
#' @importFrom tools file_ext
#' @export
#' 
toolGetMapping <- function(name, type=NULL, where="mappingfolder", error.missing=TRUE, returnPathOnly=FALSE, activecalc=NULL) {
  if(where=="mappingfolder") {
    mf <- getConfig("mappingfolder")
    if(is.null(mf)) stop('No mappingfolder specified in used cfg! Please load a config with the corresponding information!')
    fname <- paste0(mf,"/",type,"/",name)
    if(!file.exists(fname) & file.exists(system.file("extdata", name, package = "madrat"))) {
      vcat(-2,"copy mapping",name,"from madrat package into mappings folder...")
      if(!file.exists(paste0(mf,"/",type))) dir.create(paste0(mf,"/",type), recursive = TRUE)
      file.copy(system.file("extdata", name, package = "madrat"), fname)
    }
    if (file.exists(name)) {
      fname <- name
    } else if (!file.exists(fname)) {
      packages <- getConfig("packages")
      if (!is.null(activecalc)) {
        fp <- as.character(attr(prepFunctionName(activecalc,"calc"),"package"))
        packages <- c(fp,grep(fp,packages,invert = TRUE,value=TRUE))
      }
      for (i in packages) {
        out <- toolGetMapping(name, type = type, where = i, error.missing = FALSE, returnPathOnly = TRUE)
        if (out!="") {
          fname <- out
          break
          }
      }
    }
    if(error.missing & !file.exists(fname)) {
      if(!file.exists(mf)) stop('The mappings folder "', mf, '" does not exist!')
      if(!file.exists(paste0(mf,"/",type))) stop('Unknown mappings type "',type,'"!')
      stop('Mapping "',name,'" not found!')
    }
  } else if(where=="local") {
    if(is.null(type)) {
      fname <- name
    } else {
      fname <- paste0(type,"/",name)  
    }
  } else {
    if(is.null(type)) {
      tmpfname <- name
    } else {
      tmpfname <- paste0(type,"/",name)  
    }
    fname <-  system.file("extdata", tmpfname, package=where)
    if(fname=="") fname <- system.file("inst/extdata", tmpfname, package=where)
    if(fname=="" & error.missing) stop('Mapping "',name,'" with type "',type,'" not found in package "',where,'"!')
  } 
  fname <- gsub("/+","/",fname)
  if(returnPathOnly) return(fname)
  filetype <- tolower(file_ext(fname))
  if(filetype=="csv") {
    if(grepl(pattern = ";",x=readLines(fname,1))){
      return(read.csv(fname,sep = ";", stringsAsFactors = FALSE, comment.char = "*"))
    } else {
      return(read.csv(fname,sep = ",", stringsAsFactors = FALSE, comment.char = "*"))
    }
  } else if(filetype=="rda") {
    data <- NULL
    load(fname)
    if(is.null(data)) stop(fname," did not contain a object named \"data\"!")
    return(data)
  } else if(filetype=="rds") {
    return(readRDS(fname))
  } else if(filetype=="") {
    if (error.missing) stop("Unsupported filetype \"", filetype,"\"")
  } else {
    stop("Unsupported filetype \"", filetype,"\"")
  }
}
