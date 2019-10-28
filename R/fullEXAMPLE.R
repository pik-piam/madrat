#' fullExample
#' 
#' Example for class of fullX functions. Can be used as template for a new function
#' or for testing the basic functionality
#' 
#' @param rev data revision which should be used as input (positive numeric).
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Jan Philipp Dietrich
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' ,\code{\link{file2destination}}
#' @examples
#' 
#' \dontrun{ 
#' retrieveData("Test",rev=12,regionmapping="regionmappingH12.csv")
#' }
#' 
fullEXAMPLE <- function(rev=0) {
  #ATTENTION: name of the model in function name must be in capital letters!
  
  writeLines("This is a test",paste0(getConfig("outputfolder"),"/test.txt"))
  file2destination("test.txt","testfolder")
  
  if(rev>=1) {
    calcOutput("TauTotal", years=1995, round=2, file="fm_tau1995.cs4", destination="testfolder/input")
  }
  
}
