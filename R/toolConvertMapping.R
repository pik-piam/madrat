#' Tool: ConvertMapping
#' 
#' Function which converts mapping files between formats
#' 
#' 
#' @param name File name of the mapping file. Supported file types are currently csv (, or ; separated) 
#' and rda (which needs to have the data stored with the object name "data"!). Use code{\link{toolConvertMapping}}
#' to convert between both formats
#' @param format format it should be converted to. Available is "csv" or "rda".
#' @param type Mapping type (e.g. "regional", "cell", or "sectoral"). Can be set to NULL if file
#' is not stored in a type specific subfolder
#' @param where location to look for the mapping, either "mappingfolder" or
#' the name of a package which contains the mapping
#' @return the mapping as a data frame
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}, \code{\link{toolConvertMapping}}
#' @examples
#' 
#' toolGetMapping("regionmappingH12.csv", where="madrat")
#' 
#' @importFrom tools file_ext
#' @importFrom utils write.csv
#' @export
#' 
toolConvertMapping <- function(name, format="rda", type=NULL, where="mappingfolder") {
  data <- toolGetMapping(name=name, type=type, where=where)
  path <- toolGetMapping(name=name, type=type, where=where, returnPathOnly = TRUE)
  fname <- sub("\\.[^\\.]*$",paste0(".",format),basename(path))
  if(format=="rda") {
    save(data,file=fname,compress="xz")  
  } else if(format=="csv") {
    write.table(data,fname,sep=";",row.names=FALSE)
  } else {
    stop("Unsupported format ",format)
  }
}
