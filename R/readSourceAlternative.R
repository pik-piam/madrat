#' readSourceAlternative
#'
#' Read in a source file and convert it to a MAgPIE object. The function is a
#' wrapper for specific functions designed for the different possible source
#' types.
#' 
#' @param read A function to read the data. Mandatory. Function name must start with 'read'. The
#' rest of the function name determines the 'type'.
#' @param correct A function to correct the data. Optional. Function name must start with 'correct'
#' and end with the type, same as for read.
#' @param covert A function to convert the data (putting it at country level). Optional. Function 
#' name must start with 'convert' and end with the type, same as for read.
#' @param subtype A character string. For some sources there are subtypes of the source, for these
#' sources the subtype can be specified with this argument. If a source does not
#' have subtypes, subtypes should not be set.
#' @param subset A character string. Similar to \code{subtype} a source can also have \code{subsets}.
#' A \code{subsets} can be used to only read part of the data. This can in particular make sense
#' for huge data sets where reading in the whole data set might be impractical or even infeasible.
#' @export
readSourceAlternative <- function(read, correct = NULL, convert = NULL, subtype = NULL, subset = NULL) {
  
  #' check passed functions follow the naming conventions
  .checkNamingConventions <- function(read, correct, convert) {
    if (!grepl("read", deparse(substitute(read)))) {
      stop(read "does not following the naming convention: it must start with 'read'.")
    }
    type <- .getType()

    if (!is.null(correct)) {
      if (!grepl("correct", deparse(substitute(correct)))) {
        stop(correct "does not follow the naming convention: it must start with 'correct'.")
      }
      if (sub(".*correct", "", deparse(subsitute(correct))) != type) {
        stop(correct "does not follow the naming convention: it must match" type)
      }
    }

    if (!is.null(convert)) {
      if (!grepl("convert", deparse(substitute(convert)))) {
        stop(convert "does not follow the naming convention: it must start with 'convert'.")
      }
      if (sub(".*convert", "", deparse(subsitute(convert))) != type) {
        stop(convert "does not follow the naming convention: it must match" type)
      }
    }
  }

  #' get string from function for caching consistency with readSource
  .getType <- function(read) {
    type <- sub(".*read", "", deparse(substitute(read)))
    return(type)
  }

  #' get string/bool from function(s) for caching consistency with readSource
  .getPrefix <- function(convert, correct) {
    if (!is.null(convert)) {prefix <- "convert"}
    else if (!is.null(correct)) {prefix <- "correct"}
    else {prefix <- "read"}
    return(prefix)
  }

  #' get other arguments used for caching
  .getArgs <- function(subtype, suset) {
    args <- NULL
    if (!is.null(subtype)) {args <- append(args, list(subtype = subtype))}
    if (!is.null(subset)) {args <- append(args, list(subset = subset))}
    return(args)
  }

  #' try to get from cache and check
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

  .tryNewDownload <- function() {
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
  }

  .clean <- function(result) {
    if (magclass::is.magpie(result$x)) {
      result$x <- clean_magpie(result$x)
    }
    return(if (supplementary) result else result$x)
  }

  #' run the actual read/correct/convert function
  .runWithLoggingandCaching <- function(x, callable, subtype, subset) {
    sourcefolder <- getSourceFolder(type, subtype)
    
    # find which arguments we have here can be used by our callable, if any
    func_args <- names(formals(callable))
    available_args <- c("subtype", "subset")[c(!is.null(subtype), !is.null(subset))]
    selected_args <- intersect(func_args, available_args)
    if (callable %in% c(correct, convert)) {
      selected_args <- append(selected_args, "x")
    }

    # no arguments available/required
    if (is.null(selected_args)) {
      setWrapperActive("wrapperChecks")
      withr::with_dir(sourcefolder, {
        x <- withMadratLogging(callable())
      })
      setWrapperInactive("wrapperChecks")
      cachePut(xList, prefix = prefix, type = type, args = args)
      return(x)
    }
    # use available/required arguments
    selected_values <- mget(selected_args)
    setWrapperActive("wrapperChecks")
    withr::with_dir(sourcefolder, {
      x <- withMadratLogging(do.call(callable, selected_values))
    })
    setWrapperInactive("wrapperChecks")
    cachePut(xList, prefix = prefix, type = type, args = args)
    return(x)
  }

  .testISO <- function(x, ) {
    isoCountry  <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
    isoCountry1 <- as.vector(isoCountry[, "x"])
    names(isoCountry1) <- isoCountry[, "X"]
    isocountries  <- robustSort(isoCountry1)
    datacountries <- robustSort(x)
    if (length(isocountries) != length(datacountries)) {
      stop("Wrong number of countries returned by ", convert, "!")
    }
    if (any(isocountries != datacountries)) {
      stop("Countries returned by ", convert, " do not agree with iso country list!")
    }
  }

  .ensureMagpie <- function(x, callable) {
    # ensure we are always working with a list with entries "x" and "class"
    xList <- if (is.list(x)) x else list(x = x, class = "magpie")

    # set class to "magpie" if not set
    if (is.null(xList$class)) xList$class <- "magpie"

    # assert return list has the expected entries
    if (!all(c("class", "x") %in% names(xList))) {
      stop('Output of "', callable,
           '" must be a MAgPIE object or a list with the entries "x" and "class"!')
    }

    if (!inherits(xList$x, xList$class)) {
      stop('Output of "', callable, '" should have class "', xList$class, '" but it does not.')
    }

    if !is.null(convert) {
      if (magclass::is.magpie(xList$x)) {
        .testISO(magclass::getItems(xList$x, dim = 1.1))
      } else {
        vcat(2, "Non-magpie objects are not checked for ISO country level.")
      }
    }
  }


  .checkNamingConventions(read=read, correct=correct, convert=convert)

  # get default subtype argument if available, otherwise NULL
  if (is.null(subtype)) {subtype <- formals(eval(read))[["subtype"]]}

  # conversions for cache search
  type <- .getType(read=read)
  prefix <- .getPrefix(convert=convert, correct=correct)
  args <- .getArgs(subtype=subtype, subset=subset)

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

  .tryNewDownload()

  # try for a perfect cache
  xList <- .getFromCache(prefix, type, args, subtype, subset)
  if (!is.null(xList)) {
    return(.clean(xList))
  }

  # try a cache of an earlier step in the preprocessing routine and run remaining steps
  if (!is.null(convert)) {
    xList <- .getFromCache(type, subtype, subset, args, "correct")
    if !is.null(xList) {
      xList <- .runWithLoggingandCaching(xList, callable=convert, subtype=subtype, subset=subset)
    }
    return(.clean(xList))
  }

  # try last cache option
  xList <- .getFromCache(type, subtype, subset, args, "read")
  # if that didn't work, run the read function
  if (!is.null(xList)) {
    xList <- .runWithLoggingandCaching(xList, callable=read, subtype=subtype, subset=subset)
  }
  # and then run the correct and/or convert functions if desired
  if (!is.null(correct)) {
    xList <- .runWithLoggingandCaching(xList, callable=correct, subtype=subtype, subset=subset)
  }
  if (!is.null(convert)) {
    xList <- .runWithLoggingandCaching(xList, callable=convert, subtype=subtype, subset=subset)
  }

  return(.clean(xList))
}
