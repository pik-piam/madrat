#' calcOutput
#'
#' Calculate a specific output for which a calculation function exists. The function is a
#' wrapper for specific functions designed for the different possible output types.
#'
#'
#' @param type output type, e.g. "TauTotal". A list of all available source
#' types can be retrieved with function \code{\link{getCalculations}}.
#' @param aggregate Boolean indicating whether output data aggregation should be performed or
#' not, "GLO" (or "glo") for aggregation to one global region,
#' "REG+GLO" (or "regglo") for a combination of regional and global data.
#' @param file A file name. If given the output is written to that file in the
#' outputfolder as specified in the config.
#' @param years A vector of years that should be returned. If set to NULL all
#' available years are returned.
#' @param round A rounding factor. If set to NULL no rounding will occur.
#' @param supplementary boolean deciding whether supplementary information such as weight should be
#' returned or not. If set to TRUE a list of elements will be returned!
#' @param append boolean deciding whether the output data should be appended in the existing file.
#' Works only when a file name is given in the function call.
#' @param warnNA boolean deciding whether NAs in the data set should create a warning or not
#' @param na_warning deprecated, please use \code{warnNA} instead
#' @param try if set to TRUE the calculation will only be tried and the script will continue even if
#' the underlying calculation failed. If set to TRUE calculation will stop with an error in such a
#' case. This setting will be overwritten by the global setting debug=TRUE, in which try will be
#' always interpreted as TRUE.
#' @param regionmapping alternative regionmapping to use for the given calculation. It will temporarily
#' overwrite the global setting just for this calculation.
#' @param ... Additional settings directly forwarded to the corresponding
#' calculation function
#' @return magpie object with the requested output data either on country or on
#' regional level depending on the choice of argument "aggregate" or a list of information
#' if supplementary is set to TRUE.
#' @note The underlying calc-functions are required to provide a list of information back to
#' \code{calcOutput}. Following list entries should be provided:
#' \itemize{
#' \item \bold{x} - the data itself as magclass object
#' \item \bold{weight} - a weight for the spatial aggregation
#' \item \bold{unit} - unit of the provided data
#' \item \bold{description} - a short description of the data
#' \item \bold{note} (optional) - additional notes related to the data
#' \item \bold{class} (optional | default = "magpie") - Class of the returned object. If set to
#' something other than "magpie" most functionality, such as aggregation or unit tests will not
#' be available and is switched off!
#' \item \bold{isocountries} (optional | default = TRUE (mostly) or FALSE (if global)) - a boolean
#' indicating whether data is in iso countries or not (the latter will deactivate several
#' features such as aggregation)
#' \item \bold{mixed_aggregation} (optional | default = FALSE) - boolean which allows for mixed
#' aggregation (weighted mean mixed with summations). If set to TRUE weight columns
#' filled with NA will lead to summation, otherwise they will trigger an error.
#' \item \bold{min} (optional) - Minimum value which can appear in the data. If provided calcOutput
#' will check whether there are any values below the given threshold and warn in this case
#' \item \bold{max} (optional) - Maximum value which can appear in the data. If provided calcOutput
#' will check whether there are any values above the given threshold and warn in this case
#' \item \bold{structure.spatial} (optional) - regular expression describing the name structure of all
#' names in the spatial dimension (e.g. "^[A-Z]\{3\}$"). Names will be checked against this regular expression and
#' disagreements will be reported via a warning.
#' \item \bold{structure.temporal} (optional) - regular expression describing the name structure of all
#' names in the temporal dimension (e.g. "^y[0-9]\{4\}$"). Names will be checked against this regular expression and
#' disagreements will be reported via a warning.
#' \item \bold{structure.data} (optional) - regular expression describing the name structure of all
#' names in the data dimension (e.g. "^[a-z]*\\\\.[a-z]*$"). Names will be checked against this regular expression and
#' disagreements will be reported via a warning.
#' \item \bold{aggregationFunction} (optional | default = toolAggregate) - Function to be used to
#' aggregate data from country to regions. The function must have the argument \code{x} for
#' the data itself and \code{rel} for the relation mapping between countries and regions and
#' must return the data as magpie object in the spatial resolution as defined in rel.
#' \item \bold{aggregationArguments} (optional) - List of additional, named arguments to be supplied
#' to the aggregation function. In addition to the arguments set here, the function will be
#' supplied with the arguments \code{x}, \code{rel} and if provided/deviating from the default
#' also \code{weight} and \code{mixed_aggregation}.
#' \item \bold{putInPUC} (optional) boolean which decides whether this calculation should be added to a puc file
#' which contains non-aggregated data and can be used to later on aggregate the data to resolutions of own choice.
#' If not set \code{calcOutput} will try to determine automatically, whether a file is being required for the puc file
#' or not, but in more complex cases (e.g. if calculations below top-level have to be run as well) this setting can
#' be used to manually tweak the puc file list. CAUTION: Incorrect settings will cause corrupt puc files,
#' so use this setting with extreme care and only if necessary.
#' }
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}, \code{\link{calcTauTotal}},
#' @examples
#' \dontrun{
#'
#' a <- calcOutput(type = "TauTotal")
#' }
#'
#' @importFrom magclass nyears nregions getComment<- getComment getYears clean_magpie write.report write.magpie
#' getCells getYears<- is.magpie dimSums
#' @importFrom utils packageDescription read.csv2 read.csv
#' @importFrom digest digest
#' @importFrom withr defer local_dir
#' @export

calcOutput <- function(type, aggregate = TRUE, file = NULL, years = NULL, round = NULL, supplementary = FALSE, # nolint
                       append = FALSE, warnNA = TRUE, na_warning = NULL, try = FALSE, regionmapping = NULL, ...) { # nolint
  argumentValues <- c(as.list(environment()), list(...))  # capture arguments for logging

  setWrapperActive("calcOutput")
  setWrapperInactive("wrapperChecks")

  # extract saveCache statement and deactivate wrapper
  saveCache <- isWrapperActive("saveCache")
  setWrapperInactive("saveCache")

  if (!is.null(na_warning)) {
    warning('Argument "na_warning" is deprecated. Please use "warnNA" instead!')
    warnNA <- na_warning
  }

  if (!dir.exists(getConfig("cachefolder"))) {
      dir.create(getConfig("cachefolder"), recursive = TRUE)
  }

  if (!is.null(regionmapping)) setConfig(regionmapping = regionmapping, .local = TRUE)

  # read region mappings check settings for aggregate
  if (aggregate != FALSE) {
    rel <- list()
    relNames <- NULL
    for (r in c(getConfig("regionmapping"), getConfig("extramappings"))) {
      rel[[r]] <- toolGetMapping(r, type = "regional", activecalc = type)
      # rename column names from old to new convention, if necessary
      if (any(names(rel[[r]]) == "CountryCode")) names(rel[[r]])[names(rel[[r]]) == "CountryCode"] <- "country"
      if (any(names(rel[[r]]) == "RegionCode")) names(rel[[r]])[names(rel[[r]]) == "RegionCode"] <- "region"
      if (is.null(rel[[r]]$global)) {
        rel[[r]]$global <- "GLO"  # add global column
      }
      relNames <- union(relNames, names(rel[[r]]))
    }
  }

  if (!is.logical(aggregate)) {
    # rename aggregate arguments from old to new convention, if necessary
    if (toupper(aggregate) == "GLO") aggregate <- "global"
    if (toupper(gsub("+", "", aggregate, fixed = TRUE)) == "REGGLO") aggregate <- "region+global"

    # Ignore columns in 'aggregate' that are not defined in one of the mappings.
    # Stop if 'aggregate' contains none of the columns defined in one of the mappings.
    aggregateSplitted <- strsplit(aggregate, "+", fixed = TRUE)[[1]]
    commonColumns <- aggregateSplitted %in% relNames
    if (all(!commonColumns)) {
      stop("None of the columns given in aggregate = ", aggregate, " could be found in the mappings!")
    } else {
      if (any(!commonColumns)) vcat(verbosity = 0, "Omitting ", aggregateSplitted[!commonColumns],
        " from aggregate = ", aggregate, " because it does not exists in the mappings.")
      # Use those columns only for aggregation that exist in either of the mappings
      aggregate <- paste0(aggregateSplitted[commonColumns], collapse = "+")
    }
  }

  # check type input
  if (!is.character(type)) stop("Invalid type (must be a character)!")
  if (length(type) != 1) stop("Invalid type (must be a single character string)!")

  .checkData <- function(x, functionname) {
    if (!is.list(x)) stop("Output of function \"", functionname,
      "\" is not list of two MAgPIE objects containing the values and corresponding weights!")
    if (is.null(x$class)) x$class <- "magpie"
    if (!is.character(x$class) || length(x$class) != 1) stop("x$class must be a single element of class",
      " character or NULL!")
    if (x$class == "magpie" && !is.magpie(x$x)) stop("Output x of function \"", functionname,
      "\" is not a MAgPIE object!")
    if (!(x$class %in% class(x$x))) stop("Output x of function \"", functionname, "\" is not of promised class \"",
      x$class, "\"!")
    if (x$class != "magpie" && !is.null(x$weight)) stop("Weights are currently not supported for objects of class \"",
      x$class, "\"!")
    if (!is.magpie(x$weight) && !is.null(x$weight)) stop("Output weight of function \"", functionname,
      "\" is not a MAgPIE object!")
    if (!is.null(x$weight)) {
      if (nyears(x$x) != nyears(x$weight) && nyears(x$weight) != 1) stop("Number of years disagree between data and ",
        "weight of function \"", functionname, "\"!")
      if (nyears(x$weight) == 1) getYears(x$weight) <- NULL
    }
    x$package <- attr(functionname, "pkgcomment")

    # read and check x$isocountries value which describes whether the data is in
    # iso country resolution or not (affects aggregation and certain checks)
    if (x$class != "magpie") {
      if (!is.null(x$isocountries) && x$isocountries != FALSE) stop("x$isocountries can only be set ",
        "if x$class==\"magpie\"")
      x$isocountries <- FALSE
    }
    if (is.null(x$isocountries)) {
      if (nregions(x$x) == 1 && (is.null(getItems(x$x, dim = 1)) || getItems(x$x, dim = 1) == "GLO")) {
        x$isocountries <- FALSE
      } else {
        x$isocountries <- TRUE
      }
    }
    if (!is.logical(x$isocountries)) stop("x$isocountries must be a logical!")
    # read and check x$mixed_aggregation value which describes whether the data is in
    # mixed aggregation (weighted mean mixed with summation) is allowed or not
    if (is.null(x$mixed_aggregation)) x$mixed_aggregation <- FALSE # nolint
    if (!is.logical(x$mixed_aggregation)) stop("x$mixed_aggregation must be a logical!")

    # check that data is returned for ISO countries except if x$isocountries=FALSE
    if (x$isocountries) {
      .countrycheck <- function(datacountries, name) {
        datacountries <- robustSort(datacountries)
        isoCountry <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
        isoCountry1 <- as.vector(isoCountry[, "x"])
        names(isoCountry1) <- isoCountry[, "X"]
        isocountries <- robustSort(isoCountry1)
        if (length(isocountries) != length(datacountries)) stop("Wrong number of countries in ", name,
          " returned by ", functionname, "!")
        if (any(isocountries != datacountries)) stop("Countries in ", name, " returned by ", functionname,
          " do not agree with iso country list!")
      }
      .countrycheck(getItems(x$x, dim = 1.1), "x")
      if (!is.null(x$weight) && nregions(x$weight) > 1) .countrycheck(getItems(x$weight, dim = 1.1), "weight")
    }
    # perform additional checks
    if (x$class != "magpie" && (!is.null(x$min) | !is.null(x$max))) stop("Min/Max checks cannot be used in combination",
      " with x$class!=\"magpie\"")
    if (!is.null(x$min) && any(x$x < x$min, na.rm = TRUE)) vcat(0, "Data returned by ", functionname,
      " contains values smaller than the predefined minimum",
      " (min = ", x$min, ")")
    if (!is.null(x$max) && any(x$x > x$max, na.rm = TRUE)) vcat(0, "Data returned by ", functionname,
      " contains values greater than the predefined maximum",
      " (max = ", x$max, ")")
    checkNameStructure <- function(x, structure, dim, class) {
      if (class != "magpie" && !is.null(structure)) stop("Structure checks cannot be used in combination",
        " with x$class!=\"magpie\"")
      if (!is.null(structure)) {
        if (is.null(getItems(x, dim))) {
          vcat(0, paste("Missing names in dimension", dim, "!"))
        } else if (!all(grepl(structure, getItems(x, dim)))) {
          vcat(0, paste0("Invalid names (dim=", dim, ', structure=\"', structure, '\"): '),
            paste(grep(structure, getItems(x, dim), value = TRUE, invert = TRUE), collapse = ", "))
        }
      }
    }
    checkNameStructure(x$x, x$structure.spatial, 1, x$class)
    checkNameStructure(x$x, x$structure.temporal, 2, x$class)
    checkNameStructure(x$x, x$structure.data, 3, x$class)

    if (x$class == "magpie") {
      if (warnNA && anyNA(x$x))  vcat(0, "Data returned by ", functionname, " contains NAs")
      if (any(is.infinite(x$x))) vcat(0, "Data returned by ", functionname, " contains infinite values")
    }
    return(x)
  }

  if (is.null(getOption("gdt_nestinglevel"))) {
    vcat(-2, "")
  }

  startinfo <- toolstartmessage("calcOutput", argumentValues, "+")
  defer({
    toolendmessage(startinfo, "-")
  })

  if (!file.exists(getConfig("outputfolder"))) dir.create(getConfig("outputfolder"), recursive = TRUE)
  local_dir(getConfig("outputfolder"))

  functionname <- prepFunctionName(type = type, prefix = "calc", ignore = ifelse(is.null(years), "years", NA))
  extraArgs <- sapply(attr(functionname, "formals"), function(x) return(eval(parse(text = x))), simplify = FALSE) # nolint
  args <- c(extraArgs, list(...))

  x <- cacheGet(prefix = "calc", type = type, args = args)

  if (!is.null(x)) {
    x <- try(.checkData(x, functionname), silent = TRUE)
    if ("try-error" %in% class(x)) {
      vcat(2, " - cache file corrupt for ", functionname, show_prefix = FALSE)
      x <- NULL
    }
  }
  if (is.null(x)) {
    debugMode <- getConfig("debug")
    setWrapperActive("wrapperChecks")
    vcat(2, " - execute function ", functionname, show_prefix = FALSE)
    if (try || debugMode) {
      x <- withMadratLogging(try(eval(parse(text = functionname)), silent = TRUE))
      if ("try-error" %in% class(x)) {
        vcat(0, as.character(attr(x, "condition")))
        return(x)
      }
    } else {
      x <- withMadratLogging(eval(parse(text = functionname)))
    }
    setWrapperInactive("wrapperChecks")
    x <- .checkData(x, functionname)
    cachePut(x, prefix = "calc", type = type, args = args)
  }

  if (is.logical(x$putInPUC)) saveCache <- x$putInPUC

  if (saveCache) {
   write(cacheName(prefix = "calc", type = type, args = args),
         file = "pucFiles", append = TRUE)
  }


  if (!is.null(years)) {
    if (x$class != "magpie") stop("years argument can only be used in combination with x$class=\"magpie\"!")
    # check that years exist in provided data
    if (!all(as.integer(sub("y", "", years)) %in% getYears(x$x, as.integer = TRUE))) {
      stop("Some years are missing in the data provided by function ", functionname, "(",
        paste(years[!(as.integer(sub("y", "", years)) %in% getYears(x$x, as.integer = TRUE))], collapse = ", "),
        ")!")
    }
    x$x <- x$x[, years, ]
    if (!is.null(x$weight)) if (nyears(x$weight) > 1) x$weight <- x$weight[, years, ]
  }

  .prepComment <- function(x, name, warning = NULL) {
    if (!is.null(x)) {
      x[1] <- paste0(" ", name, ": ", x[1])
      if (length(x) > 1) {
        x[2:length(x)] <- paste0(paste(rep(" ", 3 + nchar(name)), collapse = ""), x[2:length(x)])
      }
    } else {
      if (!is.null(warning)) {
        vcat(0, warning)
        x <- paste0(" ", name, ": not provided")
      }
    }
    return(x)
  }

  .cleanComment <- function(x, remove = c("unit", "description", "comment", "origin", "creation date", "note")) {
    # remove old descriptors
    x <- getComment(x)
    out <- grep(paste0("^ *(", paste(remove, collapse = "|"), "):"), x, value = TRUE, invert = TRUE)
    if (length(out) == 0) return(NULL)
    return(out)
  }

  unit        <- .prepComment(x$unit, "unit", paste0('Missing unit information for data set "', type, '"!'))
  description <- .prepComment(x$description, "description",
    paste0('Missing description for data set "', type,
      '"! Please add a description in the corresponding calc function!'))
  comment     <- .prepComment(.cleanComment(x$x), "comment")
  origin      <- .prepComment(paste0(gsub("\\s{2,}", " ", paste(deparse(match.call()), collapse = "")),
    " (madrat ", unname(getNamespaceVersion("madrat")), " | ", x$package, ")"),
  "origin")
  date        <- .prepComment(date(), "creation date")
  note        <- .prepComment(x$note, "note")

  # select fitting relation mappings and merge them if there is more than one
  if (aggregate != FALSE) {
    if (x$class != "magpie") stop("Aggregation can only be used in combination with x$class=\"magpie\"!")
    items <- getItems(x$x, dim = 1)

    # find mappings that have the same (setequal) items (usually ISO countries) in any column as data (items)
    # lapply loops over the mappings, vapply loops over the columns of a mapping
    columnHasItems <- lapply(rel, vapply, setequal, items, FUN.VALUE = logical(1))
    # extract the names of columns that have the same items as data
    columnNameWithItems <- lapply(columnHasItems, function(x) names(which(x)))

    # mappings must have the same length AND the same items as data
    relFitting <- which(vapply(rel, nrow, FUN.VALUE = integer(1)) == length(items) &
                        !vapply(columnNameWithItems, identical, character(0), FUN.VALUE = logical(1)))

    if (length(relFitting) == 0) stop("Neither getConfig(\"regionmapping\") nor getConfig(\"extramappings\")",
      " contain a mapping compatible to the provided data!")

    # inform about mappings that don't fit and will be omitted
    omit <- setdiff(names(rel), names(relFitting))
    if (length(omit) > 0) {
      vcat(verbosity = 2, "Ignoring region mapping ", omit, " because it does not fit the data")
    }

    # keep mappings only that fit the data
    rel <- rel[relFitting]

    # if there is only one fitting mapping make rel a data frame
    if (length(rel) == 1) {
      rel <- rel[[1]]
    }

    # If there are more than one fitting mappings merge them. If column names from the first mapping (given via
    # 'regionmapping') also exist in further mappings (provided via 'extramappings') keep only the columns from
    # the first mapping
    if (length(relFitting) > 1) {
      itemCol <- columnNameWithItems[relFitting]
      tmp <- rel[[1]]
      for (i in 2:length(rel)) {
        # merge two mappings by their column that matched the data (see above; usually the ISO countries) and
        # append '-remove' to the names of columns in the second mapping that also exist in the first mapping.
        tmp <- merge(tmp, rel[[i]], by.x = itemCol[[1]], by.y = itemCol[[i]], suffixes = c("", "-remove"))
        # find index of columns that will be removed from the merge result
        ignoredColumnsID <- grep("-remove", colnames(tmp))
        # list names of columns that will be removed
        ignoredColumnsName <- paste(gsub("-remove", "", colnames(tmp)[ignoredColumnsID]), collapse = ", ")
        vcat(verbosity = 1, "Ignoring column(s) ", ignoredColumnsName, " from ", names(rel[i]),
             " as the column(s) already exist in another mapping.", sep = " ")
        # remove columns from the merge result tagged with '-remove'
        tmp <- tmp[, -ignoredColumnsID]
      }
      rel <- tmp
    }
  }

  # read and check x$aggregationFunction value which provides the aggregation function
  # to be used.
  if (is.null(x$aggregationFunction)) x$aggregationFunction <- "toolAggregate"
  if (!is.function(x$aggregationFunction) && !is.character(x$aggregationFunction)) {
    stop("x$aggregationFunction must be a function!")
  }

  # read and check x$aggregationArguments value which provides additional arguments
  # to be used in the aggregation function.
  if (is.null(x$aggregationArguments)) x$aggregationArguments <- list()
  if (!is.list(x$aggregationArguments)) stop("x$aggregationArguments must be a list of function arguments!")
  # Add base arguments to the argument list (except of rel, which is added later)
  x$aggregationArguments$x <- quote(x$x)
  if (!is.null(x$weight)) x$aggregationArguments$weight <- quote(x$weight)
  if (x$mixed_aggregation) x$aggregationArguments$mixed_aggregation <- TRUE # nolint

  if (aggregate != FALSE) {
    x$aggregationArguments$rel <- quote(rel)
    if (aggregate != TRUE) x$aggregationArguments$to <- aggregate
    if (try || getConfig("debug") == TRUE) {
      x$x <- try(do.call(x$aggregationFunction, x$aggregationArguments), silent = TRUE)
      if ("try-error" %in% class(x$x)) {
        vcat(0, as.character(attr(x$x, "condition")))
        return(NULL)
      }
    } else {
      x$x <- do.call(x$aggregationFunction, x$aggregationArguments)
    }
    x$x <- toolOrderCells(x$x)
  }

  if (!is.null(years)) {
    if (length(years) == 1) getYears(x$x) <- NULL
  }
  if (!is.null(round)) {
    if (x$class != "magpie") stop("rounding can only be used in combination with x$class=\"magpie\"!")
    x$x <- round(x$x, round)
  }

  extendedComment <- c(description,
    unit,
    note,
    comment,
    origin,
    date)
  if (x$class == "magpie") {
    getComment(x$x) <- extendedComment
    x$x <- clean_magpie(x$x)
  } else {
    attr(x$x, "comment") <- extendedComment
  }

  if (is.null(file) & append) {
    vcat(0, "The parameter append=TRUE works only when the file name is provided in the calcOutput() function call.")
  }

  if (!is.null(file)) {
    if (x$class == "magpie") {
      if (grepl(".mif", file) == TRUE) {
        if (!is.null(getYears(x$x))) {
          write.report(x$x, file = paste(getConfig("outputfolder"), file, sep = "/"), unit = x$unit, append = append)
        } else {
          vcat(0, "Time dimension missing and data cannot be written to a mif-file. Skip data set!")
        }
      } else {
        write.magpie(x$x, file_folder = getConfig("outputfolder"), file_name = file, mode = "777")
      }
    } else {
      if ((grepl(".rds$", file) == TRUE)) saveRDS(x$x, paste(getConfig("outputfolder"), file, sep = "/"))
      else stop("Unsupported file format (\"", file, "\") for x$class!=\"magpie\"")
    }
  }
  if (supplementary) {
    return(x)
  } else {
    return(x$x)
  }
}
