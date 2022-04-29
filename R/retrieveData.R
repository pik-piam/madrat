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
#' @param strict Boolean which allows to trigger a strict mode. During strict mode
#' warnings will be taken more seriously and will cause 1. to have the number of
#' warnings as prefix of the created tgz file and 2. will prevent \code{retrieveData}
#' from creating a puc file.
#' @param renv Boolean which determines whether calculations should run
#' within a renv environment (recommended) or not (currently only applied in
#' \code{\link{pucAggregate}}). If activated, \code{renv} will check which packages
#' in which versions were used to create the puc file, download, install and
#' load these packages and run the aggregation with them. Otherwise, the packages
#' in the currently used environment are being used.
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
#' @importFrom withr with_dir with_tempdir local_options
#' @importFrom renv snapshot
#' @export
#'

retrieveData <- function(model, rev = 0, dev = "", cachetype = "rev", puc = identical(dev, ""),
                         strict = FALSE, renv = TRUE, ...) {

  argumentValues <- c(as.list(environment()), list(...)) # capture arguments for logging

  setWrapperActive("retrieveData")
  setWrapperActive("saveCache")
  setWrapperInactive("wrapperChecks")

  local_options(madratWarningsCounter = 0)

  if (!(cachetype %in% c("rev", "def"))) {
    stop("Unknown cachetype \"", cachetype, "\"!")
  }

  rev <- numeric_version(rev)

  # check and structure settings
  cfg <- .prepConfig(model, rev, dev, ...)

  if (getConfig("debug") != TRUE) {

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
        do.call(pucAggregate, c(list(puc = file.path(getConfig("pucfolder"), matchingPUCs)), renv = renv,
                                 cfg$input[cfg$pucArguments]))
        return()
      }
    }

  }

  # data not yet ready and has to be prepared first
  outputfolder <- file.path(getConfig("outputfolder"), cfg$collectionName)

  # create folder if required
  dir.create(outputfolder, recursive = TRUE, showWarnings = !file.exists(outputfolder))

  # copy mappings to mapping folder and set config accordingly
  regionmapping <- sapply(c(getConfig("regionmapping"), getConfig("extramappings")), toolGetMapping, type = "regional", returnPathOnly = TRUE) # nolint
  mappath <- sapply(paste0(cfg$regionscode, ".csv"), toolGetMapping, "regional", error.missing = FALSE, returnPathOnly = TRUE) # nolint

  for (i in seq_along(regionmapping)) {

    # copy mapping to mapping folder
    if (!file.exists(mappath[i])) {
      dir.create(dirname(mappath[i]), recursive = TRUE, showWarnings = !dir.exists(dirname(mappath[i])))
      file.copy(regionmapping[i], mappath[i])
    }

    # copy mapping to output folder
    tryCatch({
      file.copy(regionmapping[i], outputfolder, overwrite = TRUE)
    },
    error = function(error) {
      warning("Copying regionmapping to output folder failed: ", error)
    })
  }

  tryCatch({
    saveRDS(list(package = attr(cfg$functionName, "package"),
                 args = argumentValues[!(names(argumentValues) %in% c(names(cfg$setConfig), "renv"))],
                 pucArguments = cfg$pucArguments, sessionInfo = sessionInfo()),
            file.path(outputfolder, "config.rds"), version = 2)
  },
  error = function(error) {
    warning("Creation of config.rds failed: ", error)
  })
  localConfig(regionmapping = paste0(cfg$regionscode[1], ".csv"),
              outputfolder = outputfolder,
              diagnostics = "diagnostics")

  if (length(cfg$regionscode) > 1) {
    localConfig(extramappings = paste0(cfg$regionscode[-1], ".csv")
    )
  }

  if (cachetype == "rev") {
    localConfig(cachefolder = file.path(getConfig("mainfolder"), "cache", paste0("rev", rev, dev)),
                forcecache = TRUE)
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

  with_dir(outputfolder, {
    setWrapperActive("wrapperChecks")
    returnedValue <- withMadratLogging(do.call(cfg$functionCode, cfg$fullNow))
    setWrapperInactive("wrapperChecks")
  })
  if ("tag" %in% names(returnedValue)) {
    if (grepl(pattern = "debug", returnedValue$tag)) {
      warning("The tag returned in a fullXYZ function should not include the word 'debug'")
    }
    cfg$collectionName <- paste0(cfg$collectionName, "_", returnedValue$tag)
  }

  nWarn <-  getOption("madratWarningsCounter")
  if (strict && nWarn > 0) {
    cfg$collectionName <- paste0("WARNINGS", nWarn, "_", cfg$collectionName)
    if (puc) {
      puc <- FALSE
      vcat(0, "puc file not written as ", nWarn, " warning(s) occurred and strict mode is active")
    }
  }

  vcat(2, " - function ", cfg$functionName, " finished", fill = 300, show_prefix = FALSE)

  if (puc) {
    vcat(2, " - bundling starts", fill = 300, show_prefix = FALSE)
    pucFiles <- file.path(outputfolder, "pucFiles")
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
            file.copy(file.path(outputfolder, otherFiles), ".")
            vcat(3, capture.output(snapshot(packages = attr(cfg$functionName, "package"), prompt = FALSE)))
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

  with_dir(outputfolder, {
    suppressWarnings(tar(file.path("..", paste0(cfg$collectionName, ".tgz")), compression = "gzip"))
  })
  unlink(outputfolder, recursive = TRUE)

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

  cfg$collectionName <- paste0("rev", rev, dev, "_", paste(cfg$regionscode, collapse = "-"), "_",
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

.match <- function(folder, fileType, pattern) {
  match <- dir(path = folder, pattern = paste0(".*\\.", fileType))
  match <- match[(startsWith(match, paste0(pattern, "_")) | startsWith(match, paste0(pattern, "."))) &
                   !startsWith(match, paste0(pattern, "_debug"))]
  return(match)
}
