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
#' @param where location to look for the mapping, either "mappingfolder", "local" (if the path is relative to your
#' current directory) or the name of a package which contains the mapping. If set to NULL it will first try "local",
#' then "mappingfolder" and afterwards scan all packages currently listed in \code{getConfig("packages")}
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
#' head(toolGetMapping("regionmappingH12.csv", where = "madrat"))
#' @importFrom tools file_ext
#' @importFrom pkgload is_dev_package
#' @export
toolGetMapping <- function(name, type = NULL, where = NULL,
                           error.missing = TRUE, # nolint
                           returnPathOnly = FALSE, activecalc = NULL) {

  setWrapperInactive("wrapperChecks")

  if (is.null(where)) {
    mf <- getConfig("mappingfolder")
    fname <- paste0(mf, "/", type, "/", name)
    if (file.exists(as.character(name))) {
      fname <- name
    } else if (!file.exists(as.character(fname))) {
      packages <- getConfig("packages")
      if (!is.null(activecalc[[1]]) & any(grepl(paste0("^", activecalc[[1]], "$"), getCalculations()[, "type"]))) {
        fp <- as.character(attr(prepFunctionName(activecalc[[1]], "calc"), "package"))
        packages <- c(fp, grep(fp, packages, invert = TRUE, value = TRUE))
      }
      for (i in packages) {
        out <- toolGetMapping(name, type = type, where = i, error.missing = FALSE, returnPathOnly = TRUE)
        if (out != "") {
          fname <- out
          break
        }
      }
    }
  } else if (where == "mappingfolder") {
    mf <- getConfig("mappingfolder")
    fname <- paste0(mf, "/", type, "/", name)
  } else if (where == "local") {
    if (is.null(type)) {
      fname <- name
    } else {
      fname <- paste0(type, "/", name)
    }
  } else {
    if (is.null(type)) {
      tmpfname <- name
    } else {
      tmpfname <- paste0(type, "/", name)
    }
    fname <-  system.file("extdata", tmpfname, package = where)
    if (fname == "" && !is_dev_package(where)) fname <- system.file("inst/extdata", tmpfname, package = where)
    if (fname == "") fname <- system.file("extdata", strsplit(tmpfname, split = "/")[[1]][2], package = where)
    if (fname == "" && !is_dev_package(where))
      fname <- system.file("inst/extdata", strsplit(tmpfname, split = "/")[[1]][2], package = where)
    if (fname == "" & error.missing) {
      stop('Mapping "', name, '" with type "', type, '" not found in package "', where, '"!')
    }
  }
  if (error.missing & !file.exists(as.character(fname))) stop('Mapping "', name, '" not found!')
  fname <- gsub("/+", "/", fname)
  if (returnPathOnly) return(fname)
  filetype <- tolower(file_ext(fname))
  if (filetype == "csv") {
    if (grepl(pattern = ";", x = readLines(fname, 1))) {
      return(read.csv(fname, sep = ";", stringsAsFactors = FALSE, comment.char = "*"))
    } else {
      return(read.csv(fname, sep = ",", stringsAsFactors = FALSE, comment.char = "*"))
    }
  } else if (filetype == "rda") {
    data <- NULL
    load(fname)
    if (is.null(data)) stop(fname, " did not contain a object named \"data\"!")
    return(data)
  } else if (filetype == "rds") {
    return(readRDS(fname))
  } else {
    stop("Unsupported filetype \"", filetype, "\"")
  }
}
