#' retrieveData
#'
#' Function to retrieve a predefined collection of calculations for a specific
#' regionmapping.
#'
#'
#' @param model The names of the model for which the data should be provided
#' (e.g. "magpie").
#' @param rev data revision which should be used/produced. Format must be compatible to
#' \code{\link[base]{numeric_version}}.
#' @param dev development suffix to distinguish development versions for the same data
#' revision. This can be useful to distinguish parallel lines of development.
#' @param cachetype defines what cache should be used. "rev" points to a cache
#' shared by all calculations for the given revision and sets forcecache to TRUE,
#' "def" points to the cache as defined in the current settings and does not change
#' forcecache setting.
#' @param puc Boolean deciding whether a fitting puc file (if existing) should be
#' read in and if a puc file (if not already existing) should be created.
#' @param ... (Optional) Settings that should be changed using \code{setConfig}
#' (e.g. regionmapping). or arguments which should be forwarded to the corresponding
#' fullXYZ function (Please make sure that argument names in full functions do not
#' match settings in \code{setConfig}!)
#' @note The underlying full-functions can optionally provide a list of information back to
#' \code{retrieveData}. Following list entries are currently supported:
#' \itemize{
#' \item \bold{tag} (optional) - additional name tag which will be included in the file
#' name of the aggregated collection (resulting tgz-file). This can be useful to highlight
#' information in the file name which otherwise would not be visible.
#' \item \bold{pucTag} (optional) - identical purpose as \bold{tag} but for the resulting
#' unaggregated collections (puc-files).
#' }
#' @author Jan Philipp Dietrich, Lavinia Baumstark
#' @seealso
#' \code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#' \dontrun{
#' retrieveData("example", rev = "2.1.1", dev = "test", regionmapping = "regionmappingH12.csv")
#' }
#' @importFrom methods formalArgs
#' @importFrom utils sessionInfo tar modifyList
#' @importFrom withr with_dir with_tempdir
#' @export
retrieveData <- function(model, rev = 0, dev = "", cachetype = "rev", puc = identical(dev, ""), ...) { # nolint
  argumentValues <- c(as.list(environment()), list(...)) # capture arguments for logging

  setWrapperActive("retrieveData")
  setWrapperActive("saveCache")
  setWrapperInactive("wrapperChecks")

  if (!(cachetype %in% c("rev", "def"))) {
    stop("Unknown cachetype \"", cachetype, "\"!")
  }

  rev <- numeric_version(rev)

  # check and structure settings
  cfg <- .prepConfig(model, rev, dev, ...)

  if (getConfig("debug") != TRUE) {

    .match <- function(folder, fileType, pattern) {
      match <- dir(path = folder, pattern = paste0(".*\\.", fileType))
      match <- match[startsWith(match, pattern) & !startsWith(match, paste0(pattern, "_debug"))]
      return(match)
    }

    matchingCollections <- .match(getConfig("outputfolder"), "tgz", cfg$collectionName)

    if (length(matchingCollections) > 0) {
      startinfo <- toolstartmessage("retrieveData", argumentValues, 0)
      vcat(-2, " - data is already available and not calculated again.", fill = 300)
      toolendmessage(startinfo)
      return()
    }

    if (puc) {
      matchingPUCs <- .match(getConfig("pucfolder"), "puc", cfg$pucName)

      if (length(matchingPUCs) == 1) {
        vcat(-2, " - data will be created from existing puc (", matchingPUCs, ").", fill = 300)
        do.call(pucAggregate, c(list(puc = file.path(getConfig("pucfolder"), matchingPUCs)),
                                 cfg$input[cfg$pucArguments]))
        return()
      }
    }

  }

  # data not yet ready and has to be prepared first
  sourcefolder <- file.path(getConfig("outputfolder"), cfg$collectionName)

  # create folder if required
  if (!file.exists(sourcefolder)) {
    dir.create(sourcefolder, recursive = TRUE)
  }

  # copy mapping to mapping folder and set config accordingly
  regionmapping <- toolGetMapping(getConfig("regionmapping"), type = "regional", returnPathOnly = TRUE)
  mappath <- toolGetMapping(paste0(cfg$regionscode, ".csv"), "regional", error.missing = FALSE, returnPathOnly = TRUE)
  if (!file.exists(mappath)) {
    if (!dir.exists(dirname(mappath))) {
      dir.create(dirname(mappath), recursive = TRUE)
    }
    file.copy(regionmapping, mappath)
  }
  # copy mapping to output folder
  tryCatch({
    file.copy(regionmapping, sourcefolder, overwrite = TRUE)
  }, error = function(error) {
    warning("Copying regionmapping to output folder failed: ", error)
  })
  tryCatch({
    saveRDS(list(package = attr(cfg$functionName, "package"),
                 args = argumentValues[!(names(argumentValues) %in% names(cfg$setConfig))],
                 pucArguments = cfg$pucArguments, sessionInfo = sessionInfo()),
            file.path(sourcefolder, "config.rds"), version = 2)
  }, error = function(error) {
    warning("Creation of config.rds failed: ", error)
  })
  setConfig(
    regionmapping = paste0(cfg$regionscode, ".csv"),
    outputfolder = sourcefolder,
    diagnostics = "diagnostics",
    .local = TRUE
  )

  if (cachetype == "rev") {
    setConfig(
      cachefolder = file.path(getConfig("mainfolder"), "cache", paste0("rev", rev, dev)),
      forcecache = TRUE,
      .local = TRUE
    )
  }

  getConfig(print = TRUE)

  # log sessionInfo
  vcat(3, paste(c("sessionInfo:", capture.output(sessionInfo()), "\n"), collapse = "\n"))

  # run full* functions
  startinfo <- toolstartmessage("retrieveData", argumentValues, 0)

  vcat(2, " - execute function ", cfg$functionName, fill = 300, show_prefix = FALSE)

  # get madrat graph to check for possible problems
  getMadratGraph(packages = getConfig("packages"),
                 globalenv = getConfig("globalenv"))

  with_dir(sourcefolder, {
    setWrapperActive("wrapperChecks")
    returnedValue <- do.call(cfg$functionCode, cfg$fullNow)
    setWrapperInactive("wrapperChecks")
  })
  if ("tag" %in% names(returnedValue)) {
    if (grepl(pattern = "debug", returnedValue$tag)) {
      warning("The tag returned in a fullXYZ function should not include the word 'debug'")
    }
    cfg$collectionName <- paste0(cfg$collectionName, "_", returnedValue$tag)
  }

  vcat(2, " - function ", cfg$functionName, " finished", fill = 300, show_prefix = FALSE)

  if (puc) {
    vcat(2, " - bundling starts", fill = 300, show_prefix = FALSE)
    pucFiles <- file.path(sourcefolder, "pucFiles")
    if (file.exists(pucFiles)) {
      vcat(2, " - list of files for puc identified", fill = 300, show_prefix = FALSE)
      if ("pucTag" %in% names(returnedValue)) {
        cfg$pucName <- paste0(cfg$pucName, "_", returnedValue$pucTag)
      }
      pucName <- paste0(cfg$pucName, ".puc")
      pucPath <- file.path(getConfig("pucfolder"), pucName)
      if (!dir.exists(getConfig("pucfolder"))) dir.create(getConfig("pucfolder"), recursive = TRUE)
      if (!file.exists(pucPath)) {
        cacheFiles <- readLines(pucFiles)
        if (all(file.exists(cacheFiles))) {
          vcat(1, " - create puc (", pucPath, ")", fill = 300, show_prefix = FALSE)
          with_tempdir({
            file.copy(cacheFiles, ".")
            otherFiles <- c("config.rds", "diagnostics.log", "diagnostics_full.log")
            file.copy(file.path(sourcefolder, otherFiles), ".")
            suppressWarnings(tar(pucPath, compression = "gzip"))
          })
        } else {
          vcat(1, "puc file not created: could not find all relevant files.")
        }
      }
    } else {
      vcat(1, "puc file not created: could not find list of files to be added.")
    }
  }

  with_dir(sourcefolder, {
    suppressWarnings(tar(file.path("..", paste0(cfg$collectionName, ".tgz")), compression = "gzip"))
  })
  unlink(sourcefolder, recursive = TRUE)

  toolendmessage(startinfo)
}


.prepConfig <- function(model, rev, dev, ...) {
  cfg  <- list(input = c(list(dev = dev, rev = rev), list(...)))

  # get setConfig arguments
  tmp <- intersect(names(cfg$input), formalArgs(setConfig))
  if (length(tmp) > 0) {
    cfg$setConfig <- cfg$input[tmp]
  }

  if (!is.null(cfg$setConfig)) {
    do.call(setConfig, c(cfg$setConfig, list(.local = parent.frame())))
  }

  # receive function name and code
  cfg$functionName <- prepFunctionName(type = toupper(model), prefix = "full")
  cfg$functionCode <- eval(parse(text = sub("\\(.*$", "", cfg$functionName)))

  cfg$fullDefault <- formals(cfg$functionCode)

  # compute arguments which will be sent to fullXYZ function
  if (!is.null(cfg$fullDefault)) {
    cfg$fullNow <- modifyList(cfg$fullDefault, cfg$input, keep.null = TRUE)
    cfg$fullNow <- cfg$fullNow[names(cfg$fullDefault)]
  } else {
    cfg$fullNow <- list()
  }

  # are all arguments used somewhere? -> error
  tmp <- (names(cfg$input) %in% c(formalArgs(setConfig), names(cfg$fullDefault), "rev", "dev"))
  if (!all(tmp)) {
    stop("Unknown argument(s) \"", paste(names(cfg$input)[!tmp], collapse = "\", \""), "\" in retrieveData")
  }

  # are arguments used in both - setConfig and the fullFunction? -> warning
  tmp <- intersect(formalArgs(setConfig), names(cfg$fullDefault))
  if (length(tmp) > 0) {
    warning(
      "Overlapping arguments between setConfig and retrieve function (\"",
      paste(tmp, collapse = '", "'), '")')
  }

  # create argument hash
  cfg$formalsReduced <- cfg$fullNow
  cfg$formalsReduced$dev <- NULL
  cfg$formalsReduced$rev <- NULL

  # compute which arguments can be selected when compiling a puc
  cfg$pucArguments <- getFlags(cfg$functionCode)$pucArguments$code
  cfg$formalsPUC <- cfg$formalsReduced[!(names(cfg$formalsReduced) %in% cfg$pucArguments)]

  useLabels <- !(isTRUE(getConfig("nolabels")) || model %in% getConfig("nolabels"))

  .argsHash <- function(formals, useLabels) {
    if (length(formals) > 0) {
      hashs <- digest(formals, algo = getConfig("hash"))
      if (useLabels) {
        hashs <- toolCodeLabels(hashs)
      }
      argsHash <- paste0(hashs, "_")
    } else {
      argsHash <- NULL
    }
    return(argsHash)
  }

  cfg$regionscode <- regionscode(label = useLabels)

  cfg$collectionName <- paste0("rev", rev, dev, "_", cfg$regionscode, "_",
                               .argsHash(cfg$formalsReduced, useLabels), tolower(model),
                               ifelse(getConfig("debug") == TRUE, "_debug", ""))
  if (is.null(cfg$pucArguments)) {
    extraArgs <- ""
  } else {
    extraArgs <- paste0(paste(cfg$pucArguments, collapse = "_"), "_")
  }
  cfg$pucName     <- paste0("rev", rev, dev, "_", extraArgs, .argsHash(cfg$formalsPUC, useLabels),
                               tolower(model), ifelse(getConfig("debug") == TRUE, "_debug", ""))

  return(cfg)
}
