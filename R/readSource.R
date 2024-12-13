#' readSource
#'
#' Read in a source file and convert it to a MAgPIE object. The function is a
#' wrapper for specific functions designed for the different possible source
#' types.
#'
#' @note If a magpie object is returned magclass::clean_magpie is run and if convert = TRUE
#' ISO code country level is checked.
#'
#' @param type A character string referring to the source type, e.g. "IEA" which would
#' internally call a function called `readIEA` (the "wrapped function"). A list of
#' available source types can be retrieved with function \code{\link{getSources}}.
#' @param subtype A character string. For some sources there are subtypes of the source, for these
#' sources the subtype can be specified with this argument. If a source does not
#' have subtypes, subtypes should not be set.
#' @param subset A character string. Similar to \code{subtype} a source can also have \code{subsets}.
#' A \code{subsets} can be used to only read part of the data. This can in particular make sense
#' for huge data sets where reading in the whole data set might be impractical or even infeasible.
#' @param convert Boolean indicating whether input data conversion to
#' ISO countries should be done or not. In addition it can be set to "onlycorrect"
#' for sources with a separate correctXXX-function.
#' @param supplementary Boolean deciding whether a list including the actual data and metadata,
#' or just the actual data is returned.
#' @return The read-in data, usually a magpie object. If supplementary is TRUE a list including
#' the data and metadata is returned instead. The temporal and data dimensionality
#' should match the source data. The spatial dimension should either match the source data or,
#' if the convert argument is set to TRUE, should be on ISO code country level.
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Lavinia Baumstark, Pascal Sauer
#' @seealso \code{\link{setConfig}}, \code{\link{downloadSource}}, \code{\link{readTau}}
#' #' @note The underlying read-functions can return a magpie object or a list of information
#' (preferred) back to \code{readSource}. In list format the object should have the following
#' structure:
#' \itemize{
#' \item \bold{x} - the data itself as magclass object
#' \item \bold{unit} (optional) - unit of the provided data
#' \item \bold{description} (otional) - a short description of the data
#' \item \bold{note} (optional) - additional notes related to the data
#' \item \bold{class} (optional | default = "magpie") - Class of the returned object. If set to
#' something other than "magpie" most functionality will not be available and is switched off!
#' \item \bold{cache} (optional) boolean which decides whether a cache file should be written (if caching is active)
#' or not. Default setting is TRUE. This can be for instance useful, if the calculation itself is quick, but
#' the corresponding file sizes are huge. Or if the caching for the given data type does not support storage
#' in RDS format. CAUTION: Deactivating caching for a data set which should be part of a PUC file
#' will corrupt the PUC file. Use with care.
#' }
#' @examples
#' \dontrun{
#' a <- readSource("Tau", "paper")
#' }
#'
#' @export
readSource <- function(type, subtype = NULL, subset = NULL, # nolint: cyclocomp_linter.
                       convert = TRUE, supplementary = FALSE) {
  argumentValues <- as.list(environment())  # capture arguments for logging

  setWrapperActive("readSource")
  setWrapperInactive("wrapperChecks")

  callString <- functionCallString("readSource", argumentValues)

  withr::local_dir(getConfig("mainfolder"))
  startinfo <- toolstartmessage(callString, "+")
  withr::defer({
    toolendmessage(startinfo, "-")
  })

  # check type input
  if (!is.character(type) || length(type) != 1) {
    stop("Invalid type (must be a single character string)!")
  }

  # Does the source that should be read exist?
  if (!(type %in% getSources(type = "read"))) {
    stop('Type "', type, '" is not a valid source type. Available sources are: "',
         paste(getSources(type = "read"), collapse = '", "'), '"')
  }

  # Does a correctTYPE function exist?
  if (convert == "onlycorrect" && !(type %in% getSources(type = "correct"))) {
    warning("No correct function for ", type, " could be found. Set convert to FALSE.")
    convert <- FALSE
  }

  .testISO <- function(x, functionname = "function") {
    isoCountry  <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
    isoCountry1 <- as.vector(isoCountry[, "x"])
    names(isoCountry1) <- isoCountry[, "X"]
    isocountries  <- robustSort(isoCountry1)
    datacountries <- robustSort(x)
    if (length(isocountries) != length(datacountries)) {
      stop("Wrong number of countries returned by ", functionname, "!")
    }
    if (any(isocountries != datacountries)) {
      stop("Countries returned by ", functionname, " do not agree with iso country list!")
    }
  }

  # try to get from cache and check
  .getFromCache <- function(prefix, type, args, subtype, subset) {
    xList <- cacheGet(prefix = prefix, type = type, args = args)
    if (!is.null(xList)) {
      if (!is.list(xList)) {
        xList <- list(x = xList, class = "magpie")
      }
      if (prefix == "convert" && magclass::is.magpie(xList$x)) {
        fname <- paste0(prefix, type, "_", subtype, "_", subset)
        err <- try(.testISO(magclass::getItems(xList$x, dim = 1.1), functionname = fname), silent = TRUE)
        if ("try-error" %in% class(err)) {
          vcat(2, " - cache file corrupt for ", fname, show_prefix = FALSE)
          xList <- NULL
        }
      }
    }
    return(xList)
  }

  # get data either from cache or by calculating it from source
  .getData <- function(type, subtype, subset, args, prefix, callString) {
    sourcefolder <- getSourceFolder(type, subtype)

    xList <- .getFromCache(prefix, type, args, subtype, subset)
    if (!is.null(xList)) {
      # cache hit, return
      return(xList)
    }
    # cache miss, read from source

    if (prefix != "read") {
      if (prefix == "convert" && type %in% getSources(type = "correct")) {
        upstreamPrefix <- "correct"
      } else {
        upstreamPrefix <- "read"
      }
      xList <- .getData(type, subtype, subset, args, upstreamPrefix, callString)
      # this x is passed to correct or convert function
      x <- xList$x
    }

    ignore <- c("subtype", "subset")[c(is.null(subtype), is.null(subset))]
    if (length(ignore) == 0) ignore <- NULL
    functionname <- prepFunctionName(type = type, prefix = prefix, ignore = ignore)
    setWrapperActive("wrapperChecks")

    # run the actual read/correct/convert function
    # if prefix is correct or convert the locally defined x is passed, so check it exists
    stopifnot(prefix == "read" || exists("x"))
    withr::local_dir(sourcefolder)
    x <- withMadratLogging(eval(parse(text = functionname)))
    setWrapperInactive("wrapperChecks")

    # ensure we are always working with a list with entries "x" and "class"
    xList <- if (is.list(x)) x else list(x = x, class = "magpie")

    # set class to "magpie" if not set
    if (is.null(xList$class)) xList$class <- "magpie"

    xList$package <- attr(functionname, "pkgcomment")

    # assert return list has the expected entries
    if (!all(c("class", "x") %in% names(xList))) {
      stop('Output of "', functionname,
           '" must be a MAgPIE object or a list with the entries "x" and "class"!')
    }

    if (!inherits(xList$x, xList$class)) {
      stop('Output of "', functionname, '" should have class "', xList$class, '" but it does not.')
    }

    if (prefix == "convert") {
      if (magclass::is.magpie(xList$x)) {
        .testISO(magclass::getItems(xList$x, dim = 1.1), functionname = functionname)
      } else {
        vcat(2, "Non-magpie objects are not checked for ISO country level.")
      }
    }

    extendedComment <- prepExtendedComment(xList, type, callString, warn = FALSE)
    if (xList$class == "magpie") {
      getComment(xList$x) <- extendedComment
    } else {
      attr(xList$x, "comment") <- extendedComment
    }

    cachePut(xList, prefix = prefix, type = type, args = args)
    return(xList)
  }

  # determine prefix
  if (isTRUE(convert) && (type %in% getSources(type = "convert"))) {
    prefix <- "convert"
  } else if ((isTRUE(convert) || convert == "onlycorrect") && (type %in% getSources(type = "correct"))) {
    prefix <- "correct"
  } else {
    prefix <- "read"
  }

  if (is.null(subtype)) {
    functionCall <- prepFunctionName(type = type, prefix = prefix)
    functionName <- sub("\\(.*$", "", functionCall)
    # get default subtype argument if available, otherwise NULL
    subtype <- formals(eval(parse(text = functionName)))[["subtype"]]
  }

  args <- NULL
  if (!is.null(subtype)) {
    args <- append(args, list(subtype = subtype))
  }
  if (!is.null(subset)) {
    args <- append(args, list(subset = subset))
  }

  # check forcecache before checking source dir
  forcecacheActive <- all(!is.null(getConfig("forcecache")),
                          any(isTRUE(getConfig("forcecache")),
                              type %in% getConfig("forcecache"),
                              paste0(prefix, type) %in% getConfig("forcecache")))
  if (forcecacheActive) {
    xList <- .getFromCache(prefix, type, args, subtype, subset)
    if (!is.null(xList)) {
      return(if (supplementary) xList else xList$x)
    }
  }

  # Check whether source folder exists (ignore subtype for now) and try do download source data if it is missing
  sourcefolder <- getSourceFolder(type, subtype = NULL)

  # if any DOWNLOAD.yml exists use these files as reference,
  # otherwise just check whether the sourcefolder exists
  df <- dir(sourcefolder, recursive = TRUE, pattern = "DOWNLOAD.yml")
  if (length(df) == 0) {
    sourceAvailable <- dir.exists(sourcefolder)
  } else {
    sourcefile <- file.path(sourcefolder, "DOWNLOAD.yml")
    sourcesubfile <- file.path(sourcefolder, make.names(subtype), "DOWNLOAD.yml")
    sourceAvailable <- isTRUE(file.exists(sourcefile)) || isTRUE(file.exists(sourcesubfile))
  }

  if (!sourceAvailable) {
    # does a routine exist to download the source data?
    if (type %in% getSources(type = "download")) {
      downloadCall <- prepFunctionName(type = type, prefix = "download")
      downloadFunctionName <- sub("\\(.*$", "", downloadCall)
      downloadFormals <- formals(eval(parse(text = downloadFunctionName)))
      downloadHasSubtypeArg <- "subtype" %in% names(downloadFormals)

      downloadSource(type = type, subtype = if (downloadHasSubtypeArg) subtype else NULL)
    } else {
      typesubtype <- paste0(paste(c(paste0('type = "', type), subtype), collapse = '" subtype = "'), '"')
      stop("Sourcefolder does not contain data for the requested source ", typesubtype,
           " and there is no download script which could provide the missing data. Please check your settings!")
    }
  }

  if (!is.logical(convert) && convert != "onlycorrect") {
    stop('Unknown convert setting "', convert, '" (allowed: TRUE, FALSE and "onlycorrect")')
  }

  xList <- .getData(type, subtype, subset, args, prefix, callString)
  if (magclass::is.magpie(xList$x)) {
    xList$x <- clean_magpie(xList$x)
  }
  return(if (supplementary) xList else xList$x)
}
