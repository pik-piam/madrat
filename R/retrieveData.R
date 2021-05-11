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
#' @export
retrieveData <- function(model, rev = 0, dev = "", cachetype = "rev", ...) {
  argumentValues <- c(as.list(environment()), list(...))  # capture arguments for logging

  if (!(cachetype %in% c("rev", "def"))) {
    stop("Unknown cachetype \"", cachetype, "\"!")
  }

  # extract setConfig settings and apply via setConfig
  inargs <- list(...)

  tmp <- intersect(names(inargs), formalArgs(setConfig))
  if (length(tmp) > 0) {
    do.call(setConfig, inargs[tmp])
  }

  # receive function name and function
  functionname <- prepFunctionName(type = toupper(model), prefix = "full")
  functiononly <- eval(parse(text = sub("\\(.*$", "", functionname)))

  # are all arguments used somewhere? -> error
  tmp <- (names(inargs) %in% union(formalArgs(setConfig), formalArgs(functiononly)))
  if (!all(tmp)) {
    stop("Unknown argument(s) \"", paste(names(inargs)[!tmp], collapse = "\", \""), "\"")
  }

  # are arguments used in both - setConfig and the fullFunction? -> warning
  tmp <- intersect(formalArgs(setConfig), formalArgs(functiononly))
  if (length(tmp) > 0) {
    warning(
      "Overlapping arguments between setConfig and retrieve function (\"",
      paste(tmp, collapse = "\", \""), "\")"
    )
  }

  uselabels <- !(isTRUE(getConfig("nolabels")) || model %in% getConfig("nolabels"))

  # reduce inargs to arguments sent to full function and create hash from it
  inargs <- inargs[names(inargs) %in% formalArgs(functiononly)]
  # insert default arguments, if not set explicitly to ensure identical args_hash
  defargs <- formals(functiononly)
  # remove dev and rev arguments as they are being treated separately
  defargs$dev <- NULL
  defargs$rev <- NULL
  toadd <- names(defargs)[!(names(defargs) %in% names(inargs))]
  if (length(toadd) > 0) {
    inargs[toadd] <- defargs[toadd]
  }
  if (length(inargs) > 0) {
    hashs <- digest(inargs, algo = getConfig("hash"))
    if (uselabels) {
      hashs <- toolCodeLabels(hashs)
    }
    argsHash <- paste0(hashs, "_")
  } else {
    argsHash <- NULL
  }

  regionmapping <- getConfig("regionmapping")
  if (!file.exists(regionmapping)) {
    regionmapping <- toolGetMapping(regionmapping, type = "regional", returnPathOnly = TRUE)
  }
  regionscode <- regionscode(regionmapping, label = uselabels)

  # save current settings to set back if needed
  cfgBackup <- getOption("madrat_cfg")
  on.exit(options("madrat_cfg" = cfgBackup))

  rev <- numeric_version(rev)

  collectionname <- paste0(
    "rev", rev, dev, "_", regionscode, "_", argsHash, tolower(model),
    ifelse(getConfig("debug") == TRUE, "_debug", "")
  )

  .characterInCollectionname <- function(regexCharacter) {
    return(grepl(regexCharacter, collectionname, fixed = TRUE))
  }
  if (any(unlist(lapply(strsplit("[](){}*+?|^$.\\", "")[[1]], .characterInCollectionname)))) {
    warning(paste0("At least one of the regex characters [](){}*+?|^$.\\ appeared in collectionname",
                   " - this might lead to unexpected caching behavior"))
  }

  if (length(dir(path = getConfig("outputfolder"),
                 pattern = paste0(collectionname, ".*\\.tgz"))) == 0
      || getConfig("debug") == TRUE) {
    # data not yet ready and has to be prepared first
    sourcefolder <- paste0(getConfig("outputfolder"), "/", collectionname)

    # create folder if required
    if (!file.exists(sourcefolder)) {
      dir.create(sourcefolder, recursive = TRUE)
    }

    # copy mapping to mapping folder and set config accordingly
    mappath <- toolGetMapping(paste0(regionscode, ".csv"), "regional", error.missing = FALSE, returnPathOnly = TRUE)
    if (!file.exists(mappath)) {
      if (!dir.exists(dirname(mappath))) {
        dir.create(dirname(mappath), recursive = TRUE)
      }
      file.copy(regionmapping, mappath)
    }
    # copy mapping to output folder
    try(file.copy(regionmapping, sourcefolder, overwrite = TRUE))
    setConfig(
      regionmapping = paste0(regionscode, ".csv"),
      outputfolder = sourcefolder,
      diagnostics = "diagnostics"
    )

    if (cachetype == "rev") {
      setConfig(cachefolder = paste0(getConfig("mainfolder"), "/cache/rev", rev, dev),
                forcecache = TRUE)
    }

    getConfig(print = TRUE)

    # run full* functions
    startinfo <- toolstartmessage(argumentValues, 0)

    vcat(2, " - execute function ", functionname, fill = 300, show_prefix = FALSE)

    # add rev and dev arguments
    inargs$rev <- rev
    inargs$dev <- dev

    args <- as.list(formals(functiononly))
    for (n in names(args)) {
      if (n %in% names(inargs)) {
        args[[n]] <- inargs[[n]]
      }
    }
    returnedValue <- do.call(functiononly, args)
    collectionname <- paste0(collectionname, ifelse("tag" %in% names(returnedValue), paste0("_", returnedValue$tag), ""))
    vcat(2, " - function ", functionname, " finished", fill = 300, show_prefix = FALSE)

    cwd <- getwd()
    setwd(sourcefolder)
    system(paste0("tar --create --gzip --file ../", collectionname, ".tgz", " ./*"))
    setwd(cwd)
    unlink(sourcefolder, recursive = TRUE)
  } else {
    startinfo <- toolstartmessage(argumentValues, 0)
    vcat(-2, " - data is already available and not calculated again.", fill = 300)
  }
  toolendmessage(startinfo)
}
