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
#' @param bundle Boolean deciding whether a bundle tgz should be considered and/or created, or not.
#' @param ... (Optional) Settings that should be changed using \code{setConfig}
#' (e.g. regionmapping). or arguments which should be forwarded to the corresponding
#' fullXYZ function (Please make sure that argument names in full functions do not
#' match settings in \code{setConfig}!)
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
retrieveData <- function(model, rev = 0, dev = "", cachetype = "rev", bundle = TRUE, ...) { # nolint
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

    if (bundle) {
      matchingBundles <- .match(getConfig("bundlefolder"), "bdl", cfg$bundleName)

      if (length(matchingBundles) == 1) {
        vcat(-2, " - data will be created from existing bundle (", matchingBundles, ").", fill = 300)
        do.call(bundleCompile, c(list(bundle = file.path(getConfig("bundlefolder"), matchingBundles)),
                                 cfg$input[cfg$bundleArguments]))
        return()
      }
    }

  }

  # data not yet ready and has to be prepared first
  sourcefolder <- paste0(getConfig("outputfolder"), "/", cfg$collectionName)

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
  try(file.copy(regionmapping, sourcefolder, overwrite = TRUE))
  try(saveRDS(list(package = attr(cfg$functionName, "package"), args = argumentValues,
                   bundleArguments = cfg$bundleArguments, sessionInfo = sessionInfo()),
              file.path(sourcefolder, "config.rds"), version = 2))
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
    cfg$collectionName <- paste0(cfg$collectionName, paste0("_", returnedValue$tag))
  }

  vcat(2, " - function ", cfg$functionName, " finished", fill = 300, show_prefix = FALSE)

  if (bundle) {
    vcat(2, " - bundling starts", fill = 300, show_prefix = FALSE)
    bundleFiles <- file.path(sourcefolder, "bundleFiles")
    if (file.exists(bundleFiles)) {
      vcat(2, " - list of files for bundle identified", fill = 300, show_prefix = FALSE)
      bundleName <- paste0(cfg$bundleName, ".bdl")
      bundlePath <- file.path(getConfig("bundlefolder"), bundleName)
      if (!dir.exists(getConfig("bundlefolder"))) dir.create(getConfig("bundlefolder"), recursive = TRUE)
      if (!file.exists(bundlePath)) {
        vcat(1, " - create bundle (", bundlePath, ")", fill = 300, show_prefix = FALSE)
        with_tempdir({
          cacheFiles <- readLines(bundleFiles)
          file.copy(cacheFiles, ".")
          otherFiles <- c("config.rds", "diagnostics.log", "diagnostics_full.log")
          file.copy(file.path(sourcefolder, otherFiles), ".")
          suppressWarnings(tar(bundlePath, compression = "gzip"))
        })
      }
    } else {
      vcat(1, "Could not find list of files to be bundled")
    }
  }

  with_dir(sourcefolder, {
    suppressWarnings(tar(paste0("../", cfg$collectionName, ".tgz"), compression = "gzip"))
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
      paste(tmp, collapse = "\", \""), "\")")
  }

  # create argument hash
  cfg$formalsReduced <- cfg$fullNow
  cfg$formalsReduced$dev <- NULL
  cfg$formalsReduced$rev <- NULL

  cfg$bundleArguments <- getFlags(cfg$functionCode)$bundleArguments$code
  cfg$formalsBundle <- cfg$formalsReduced[!(names(cfg$formalsReduced) %in% cfg$bundleArguments)]

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
  if (is.null(cfg$bundleArguments)) {
    extraArgs <- ""
  } else {
    extraArgs <- paste0(paste(cfg$bundleArguments, collapse = "_"), "_")
  }
  cfg$bundleName     <- paste0("rev", rev, dev, "_", extraArgs, .argsHash(cfg$formalsBundle, useLabels),
                               tolower(model), ifelse(getConfig("debug") == TRUE, "_debug", ""))

  return(cfg)
}
