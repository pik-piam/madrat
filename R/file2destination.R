#' Tool: file2destination
#' 
#' Function which maps a file of the data output folder to the
#' destination path in the corresponding folder. Mapping is stored
#' in a the data output folder in a file called 'file2destination.txt'
#' 
#' 
#' @param file The name of the file which should be mapped to a destination
#' @param destination The path relative to the main folder of the model to
#' which the file should be copied. In the case that the file should be copied
#' to more than one destination within the model data should be provided as a
#' vector of destinations.
#' 
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}, \code{\link{retrieveData}}
#' @examples
#' 
#' \dontrun{
#' file2destination("example.txt","example/folder")
#' }
#' @importFrom utils write.table read.csv
#' @export
file2destination <- function(file,destination) {
  mapfile <- paste0(getConfig("outputfolder"),"/file2destination.csv")
  map <- matrix(nrow = 0,ncol=2,dimnames=list(NULL,c("file","destination")))
  if(file.exists(mapfile)) map <- read.csv(mapfile, sep=";", stringsAsFactors = FALSE)
  for(d in destination) {
    map <- rbind(map,c(file,d))
  }
  write.table(map,mapfile,sep=";",quote=FALSE,row.names = FALSE)
}
