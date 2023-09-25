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

  if (isWrapperActive("wrapperChecks")) {
  for (w in c("downloadSource", "readSource", "calcOutput", "retrieveData")) {
    if (isWrapperActive(w)) {
      if (is.null(where)) {
        warning("argument 'where' should be set when calling toolGetMapping from within a madrat function.")
        break
      }
    }
  }
}
setWrapperInactive("wrapperChecks")
  fname <- .searchName(name = name, type = type, where = where, activecalc = activecalc)

  if (error.missing && !file.exists(as.character(fname))) stop('Mapping "', name, '" not found!')
  fname <- gsub("/+", "/", fname)
  if (returnPathOnly) return(fname)

  return(.readMapping(fname = fname))
}

.searchName <- function(name, type, where, activecalc) {
  if (is.null(where)) {
    fname <- .searchNameLocal(name, type = type)
    if (file.exists(as.character(fname))) {
      return(fname)
    }
    fname <- .searchNameMappingFolder(name, type = type)
    if (file.exists(as.character(fname))) {
      return(fname)
    }

    packages <- getConfig("packages")
    if (!is.null(activecalc[[1]]) && any(grepl(paste0("^", activecalc[[1]], "$"), getCalculations()[, "type"]))) {
      fp <- as.character(attr(prepFunctionName(activecalc[[1]], "calc"), "package"))
      packages <- c(fp, grep(fp, packages, invert = TRUE, value = TRUE))
    }
    for (i in packages) {
      out <- .searchNamePackage(name, type = type, packageName = i)
      if (out != "") {
        fname <- out
      }
    }
    # if not found in packages, fall back to result from mappingfolder, even though it doesn't exist
    return(fname)

  } else if (where == "mappingfolder") {
    return(.searchNameMappingFolder(name = name, type = type))
  } else if (where == "local") {
    return(.searchNameLocal(name = name, type = type))
  } else {
    return(.searchNamePackage(name = name, type = type, packageName = where))
  }
}

.searchNameLocal <- function(name, type) {
  if (file.exists(as.character(name))) {
    return(name)
  }
  return(.typedName(name, type))
}

.searchNameMappingFolder <- function(name, type) {
  mf <- getConfig("mappingfolder")
  return(paste0(mf, "/", type, "/", name))
}

.searchNamePackage <- function(name, type, packageName) {
  tmpfname <- .typedName(name, type)
  fname <- system.file("extdata", tmpfname, package = packageName)
  if (fname == "" && !is_dev_package(packageName)) fname <- system.file("inst/extdata", tmpfname, package = packageName)
  if (fname == "") fname <- system.file("extdata", strsplit(tmpfname, split = "/")[[1]][2], package = packageName)
  if (fname == "" && !is_dev_package(packageName))
    fname <- system.file("inst/extdata", strsplit(tmpfname, split = "/")[[1]][2], package = packageName)
  return(fname)
}

.typedName <- function(name, type) {
  if (is.null(type)) {
    return(name)
  }
  return(paste0(type, "/", name))
}

.readMapping <- function(fname) {
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
    stop("Unsupported filetype \"", filetype, "\" of file \"", fname, "\"")
  }
}
