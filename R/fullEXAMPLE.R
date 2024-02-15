#' fullExample
#'
#' Example for class of fullX functions. Can be used as template for a new function
#' or for testing the basic functionality
#'
#' @param rev data revision which should be used/produced. Will be converted to
#' \code{\link[base]{numeric_version}} when called via \code{\link{retrieveData}}.
#' @param dev development suffix to distinguish development versions for the same data
#' revision. This can be useful to distinguish parallel lines of development.
#' @param extra additional argument which - when changed - does not require a re-computation
#' of the portable unaggegrated collection (puc) file.
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Jan Philipp Dietrich
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#' \dontrun{
#' retrieveData("example", rev = "2.1.2", dev = "test", regionmapping = "regionmappingH12.csv")
#' }
#'
fullEXAMPLE <- function(rev = 0, dev = "", extra = "Example argument") {
  # ATTENTION: name of the model in function name must be in capital letters!

  "!# @pucArguments extra"

  writeLines(extra, "test.txt")

  if (rev >= numeric_version("1")) {
    calcOutput("TauTotal", years = 1995, round = 2, file = "fm_tau1995.cs4")
  }
  if (dev == "test") {
    message("Here you could execute code for a hypothetical development version called \"test\"")
  }
  # return is optional, tag is appended to the tgz filename, pucTag is appended to the puc filename
  return(list(tag = "customizable_tag",
              pucTag = "tag"))
}
