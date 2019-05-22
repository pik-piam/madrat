#' diagnosticSources
#' 
#' Function to extract sources from a diagnostics.csv file
#' 
#' 
#' @param file path to a diagnostics.csv file (as created by retrieveData)
#' @return a vector with sources detected in the diagnostics.csv 
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{retrieveData}}
#' @export 

diagnosticSources <- function(file) {
  a <- readLines(file)
  b <- grep("readSource",a,value = TRUE)
  c <- sub("^[^\"]*\"([^\"]*)\".*$","\\1",b)
  sources <- unique(c)
  folders <- getSources()
  realsources <- intersect(sources,folders)
  errors <- setdiff(sources,folders)
  if(length(errors)>0) {
    warning("Wrong detection (see print statements): ", paste(errors,collapse=", "))
    for(e in errors) {
      print(grep(paste0("\"",e,"\""),b,value = TRUE))
    }
  }
  return(sort(sources))
}
