#' Tool: fingerprint
#' 
#' Function which creates a fingerprint of a folder together with some R
#' objects (e.g. functions). Based on the fingerprint it is possible to decide
#' whether there were some changes in the given folder and the given objects or
#' not. If the fingerprint is unchanged also all files and objects stayed the
#' same, otherwise the fingerprint changes.
#' @note For a better performance not the files in a folder itself are hashed
#' but the last modified dates of these files.
#' 
#' @param folder A folder containing objects for which the fingerprint should
#' be created (all files in that folder and all sub-folders will be considered)
#' @param ... R data objects that should be considered for the fingerprint as
#' well (e.g. functions, variables,...)
#' @return A md5-based fingerprint of all provided sources
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}
#' @examples
#' madrat:::fingerprint(".",ls,c)
#' @importFrom digest digest
fingerprint <- function(folder,...) {
  # get a md5 based fingerprint of last modified dates of all files in a folder
  cwd <- getwd()
  setwd(folder)
  fp <- digest(file.mtime(sort(list.files(".",recursive=TRUE))),"md5")
  setwd(cwd)
  
  for(i in list(...)) {
    fp <- c(fp,digest(i,"md5"))
  }
  return(digest(fp,"md5"))
}
