#' readSource
#'
#' Read in a source file and convert it to a MAgPIE object. The function is a
#' wrapper for specific functions designed for the different possible source
#' types.
#'
#'
#' @param type source type, e.g. "IEA". A list of all available source types
#' can be retrieved with function \code{\link{getSources}}.
#' @param subtype For some sources there are subtypes of the source, for these
#' source the subtype can be specified with this argument. If a source does not
#' have subtypes, subtypes should not be set.
#' @param convert Boolean indicating whether input data conversion to
#' ISO countries should be done or not. In addition it can be set to "onlycorrect"
#' for sources with a separate correctXXX-function.
#' @return magpie object with the temporal and data dimensionality of the
#' source data. Spatial will either agree with the source data or will be on
#' ISO code country level depending on your choice for the argument "convert".
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Lavinia Baumstark
#' @seealso \code{\link{setConfig}}, ' \code{\link{downloadSource}},
#' \code{\link{readTau}}
#' @examples
#' \dontrun{
#' a <- readSource("Tau", "paper")
#' }
#'
#' @importFrom magclass read.magpie is.magpie getComment<- getItems
#' @importFrom methods existsFunction is
#' @importFrom withr local_dir
#' @export
readSource <- function(type, subtype = NULL, convert = TRUE) { # nolint
  argumentValues <- as.list(environment())  # capture arguments for logging
  cwd <- getwd()
  local_dir(getConfig("mainfolder"))
  startinfo <- toolstartmessage("readSource", argumentValues, "+")
  on.exit(toolendmessage(startinfo, "-"))

  # check type input
  if (!is.character(type) || length(type) != 1) stop("Invalid type (must be a single character string)!")

  # Does the source that should be read exist?
  if (!(type %in% getSources(type = "read"))) {
    stop('Type "', type, '" is not a valid source type. Available sources are: "',
      paste(getSources(type = "read"), collapse = '", "'), '"')
  }

  # Does a correctTYPE function exist?
  if (convert == "onlycorrect" & !(type %in% getSources(type = "correct"))) {
    warning("No correct function for ", type, " could be found. Set convert to FALSE.")
    convert <- FALSE
  }

  testISO <- function(x, functionname = "function") {
    isoCountry  <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
    isoCountry1 <- as.vector(isoCountry[, "x"])
    names(isoCountry1) <- isoCountry[, "X"]
    isocountries  <- robustSort(isoCountry1)
    datacountries <- robustSort(x)
    if (length(isocountries) != length(datacountries)) stop("Wrong number of countries returned by ", functionname, "!")
    if (any(isocountries != datacountries)) stop("Countries returned by ", functionname,
      " do not agree with iso country list!")
  }

  .getData <- function(type, subtype, prefix = "read") {
    # get data either from cache or by calculating it from source
    sourcefolder <- paste0(getConfig("sourcefolder"), "/", make.names(type))
    if (!is.null(subtype) && file.exists(paste0(sourcefolder, "/", make.names(subtype), "/DOWNLOAD.yml"))) {
      sourcefolder <- paste0(sourcefolder, "/", make.names(subtype))
    }

    fname <- paste0(prefix, type, subtype)
    args <- NULL
    if (!is.null(subtype)) args <- list(subtype = subtype)
    x <- cacheGet(prefix = prefix, type = type, args = args)
    if (!is.null(x) && prefix == "convert") {
      err <- try(testISO(getItems(x, dim = 1.1), functionname = fname), silent = TRUE)
      if ("try-error" %in% class(err)) {
        vcat(2, " - cache file corrupt for ", fname, show_prefix = FALSE)
        x <- NULL
      }
    }
    if (!is.null(x)) {
      return(x)
    }

    if (prefix == "correct") {
      x <- .getData(type, subtype, "read")
    } else if (prefix == "convert") {
      if (type %in% getSources(type = "correct")) {
        x <- .getData(type, subtype, "correct")
      } else {
        x <- .getData(type, subtype, "read")
      }
    }

    cwd <- getwd()
    local_dir(sourcefolder)
    functionname <- prepFunctionName(type = type, prefix = prefix, ignore = ifelse(is.null(subtype), "subtype", NA))
    x <- eval(parse(text = functionname))
    local_dir(cwd)
    if (!is.magpie(x)) stop("Output of function \"", functionname, "\" is not a MAgPIE object!")
    if (prefix == "convert") {
      testISO(getItems(x, dim = 1.1), functionname = functionname)
    }
    args <- NULL
    if (!is.null(subtype)) args <- list(subtype = subtype)
    cachePut(x, prefix = prefix, type = type, args = args)
    return(x)
  }

  # Check whether source folder exists and try do download source data if it is missing
  sourcefolder <- paste0(getConfig("sourcefolder"), "/", type)
  # if any DOWNLOAD.yml exists use these files as reference,
  # otherwise just check whether the sourcefolder exists
  df <- dir(sourcefolder, recursive = TRUE, pattern = "DOWNLOAD.yml")
  if (length(df) == 0) {
    sourceMissing <- !file.exists(sourcefolder)
  } else {
    sourcefile <- paste0(getConfig("sourcefolder"), "/", make.names(type), "/DOWNLOAD.yml")
    sourcesubfile <- paste0(getConfig("sourcefolder"), "/", make.names(type), "/", make.names(subtype), "/DOWNLOAD.yml")
    sourceMissing <- (!file.exists(sourcefile) && !file.exists(sourcesubfile))
  }

  if (sourceMissing) {
    # does a routine exist to download the source data?
    if (type %in% getSources(type = "download")) {
      downloadSource(type = type, subtype = subtype)
    } else {
      typesubtype <- paste0(paste(c(paste0("type = \"", type), subtype), collapse = "\" subtype = \""), "\"")
      stop("Sourcefolder does not contain data for the requested source ", typesubtype,
        " and there is no download script which could provide the missing data. Please check your settings!")
    }
  }

  if (!is.logical(convert) && convert != "onlycorrect") stop("Unknown convert setting \"", convert,
    "\" (allowed: TRUE, FALSE and \"onlycorrect\") ")

  if (convert == TRUE && (type %in% getSources(type = "convert"))) {
    prefix <- "convert"
  } else if (convert %in% c(TRUE, "onlycorrect") && (type %in% getSources(type = "correct"))) {
    prefix <- "correct"
  } else {
    prefix <- "read"
  }

  x <- .getData(type, subtype, prefix)
  on.exit(toolendmessage(startinfo, "-"))
  x <- clean_magpie(x)
  local_dir(cwd)
  return(x)
}
