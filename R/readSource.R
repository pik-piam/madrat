#' readSource
#'
#' Read in a source file and convert it to a MAgPIE object. The function is a
#' wrapper for specific functions designed for the different possible source
#' types.
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
#' @return The read-in data, usually a magpie object. Technically speaking
#' `if (is.list(result)) result$x else result` will be returned, where `result` is the
#' return value of the wrapped function. The temporal and data dimensionality
#' should match the source data. The spatial dimension should either match the source data or,
#' if the convert argument is set to TRUE, should be on ISO code country level. For magpie objects
#' magclass::clean_magpie is run and if convert = TRUE ISO code country level is checked.
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Lavinia Baumstark, Pascal Führlich
#' @seealso \code{\link{setConfig}}, \code{\link{downloadSource}}, \code{\link{readTau}}
#' @examples
#' \dontrun{
#' a <- readSource("Tau", "paper")
#' }
#'
#' @export
readSource <- function(type, subtype = NULL, subset = NULL, convert = TRUE) { # nolint
  argumentValues <- as.list(environment())  # capture arguments for logging

  setWrapperActive("readSource")
  setWrapperInactive("wrapperChecks")

  withr::local_dir(getConfig("mainfolder"))
  startinfo <- toolstartmessage("readSource", argumentValues, "+")
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
    x <- cacheGet(prefix = prefix, type = type, args = args)
    if (!is.null(x) && prefix == "convert") {
      fname <- paste0(prefix, type, "_", subtype, "_", subset)
      err <- try(.testISO(magclass::getItems(x, dim = 1.1), functionname = fname), silent = TRUE)
      if ("try-error" %in% class(err)) {
        vcat(2, " - cache file corrupt for ", fname, show_prefix = FALSE)
        x <- NULL
      }
    }
    return(x)
  }

  .getData <- function(type, subtype, subset, args, prefix = "read") {
    # get data either from cache or by calculating it from source
    sourcefolder <- file.path(getConfig("sourcefolder"), make.names(type))
    if (!is.null(subtype) && file.exists(file.path(sourcefolder, make.names(subtype), "DOWNLOAD.yml"))) {
      sourcefolder <- file.path(sourcefolder, make.names(subtype))
    }

    x <- .getFromCache(prefix, type, args, subtype, subset)
    if (!is.null(x)) {
      # cache hit, return
      return(x)
    }
    # cache miss, read from source

    if (prefix != "read") {
      if (prefix == "convert" && type %in% getSources(type = "correct")) {
        upstreamPrefix <- "correct"
      } else {
        upstreamPrefix <- "read"
      }
      # this x is passed if prefix is correct or convert
      x <- .getData(type, subtype, subset, args, upstreamPrefix)
    }

    withr::with_dir(sourcefolder, {
      ignore <- c("subtype", "subset")[c(is.null(subtype), is.null(subset))]
      if (length(ignore) == 0) ignore <- NULL
      functionname <- prepFunctionName(type = type, prefix = prefix, ignore = ignore)
      setWrapperActive("wrapperChecks")

      # run the actual read/correct/convert function
      # the locally defined x is passed if prefix is correct or convert
      x <- withMadratLogging(eval(parse(text = functionname)))

      setWrapperInactive("wrapperChecks")
    })

    # ensure we are always working with a list with entries "x" and "class"
    xList <- if (is.list(x)) x else list(x = x, class = "magpie")

    if (!identical(robustSort(names(xList)), c("class", "x"))) {
      stop('Output of "', functionname,
            '" must be a MAgPIE object or a list with the entries "x" and "class"!')
    }

    if (!inherits(xList$x, xList$class)) {
      stop('Output of "', functionname, '" should have class "', xList$class, '" but it does not.')
    }

    if (prefix == "convert") {
      if (xList$class == "magpie") {
        .testISO(magclass::getItems(xList$x, dim = 1.1), functionname = functionname)
      } else {
        vcat(2, "Non-magpie objects are not checked for ISO country level.")
      }
    }

    cachePut(x, prefix = prefix, type = type, args = args)
    return(x)
  }

  # determine prefix
  if (isTRUE(convert) && (type %in% getSources(type = "convert"))) {
    prefix <- "convert"
  } else if ((isTRUE(convert) || convert == "onlycorrect") && (type %in% getSources(type = "correct"))) {
    prefix <- "correct"
  } else {
    prefix <- "read"
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
    x <- .getFromCache(prefix, type, args, subtype, subset)
    if (!is.null(x)) {
      return(x)
    }
  }

  # Check whether source folder exists and try do download source data if it is missing
  sourcefolder <- file.path(getConfig("sourcefolder"), make.names(type))
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
      downloadSource(type = type, subtype = subtype)
    } else {
      typesubtype <- paste0(paste(c(paste0('type = "', type), subtype), collapse = '" subtype = "'), '"')
      stop("Sourcefolder does not contain data for the requested source ", typesubtype,
           " and there is no download script which could provide the missing data. Please check your settings!")
    }
  }

  if (!is.logical(convert) && convert != "onlycorrect") {
    stop('Unknown convert setting "', convert, '" (allowed: TRUE, FALSE and "onlycorrect")')
  }

  x <- .getData(type, subtype, subset, args, prefix)
  if (magclass::is.magpie(x)) {
    x <- clean_magpie(x)
  } else if (x$class == "magpie") {
    x$x <- clean_magpie(x$x)
  }
  return(x)
}
