#' pucAggregate
#'
#' Function which takes a puc-file ("portable unaggregated collection") as created
#' via \code{\link{retrieveData}} and computes the corresponding aggregated
#' collection with the provided arguments (e.g. the provided region mapping).
#' The resulting tgz-file containing the collection will be put to the
#' madrat outputfolder as defined in \code{getConfig("outputfolder")}.
#'
#' @param puc path to a puc-file
#' @param regionmapping region mapping to be used for aggregation.
#' @param ... (Optional) Settings that should be changed in addition. NOTE:
#' which settings can be modified varies from puc to puc. Allowed settings are
#' typically listed in the file name of the puc file after the revision number.
#' @param renv Boolean which determines whether data should be aggregated from
#' within a renv environment (recommended) or not. If activated, \code{renv}
#' will check which packages in which versions were used to create the puc file,
#' download, install and load these packages and run the aggregation with them.
#' Otherwise, the packages in the currently used environment are being used.
#' @param strict Boolean or NULL which allows to trigger a strict mode. During strict mode
#' warnings will be taken more seriously and will cause 1. to have the number of
#' warnings as prefix of the created tgz file and 2. will prevent \code{retrieveData}
#' from creating a puc file. If set to NULL the setting will be read from the puc file.
#' @author Jan Philipp Dietrich
#' @seealso
#' \code{\link{retrieveData}},\code{\link{localConfig}}
#' @examples
#' \dontrun{
#' pucAggregate("rev1_example.puc", regionmapping = "regionmappingH12.csv")
#' }
#' @importFrom withr with_tempdir local_package local_options
#' @importFrom utils untar modifyList
#' @importFrom callr r
#' @importFrom renv activate restore
#' @export
pucAggregate <- function(puc, regionmapping = getConfig("regionmapping"), ..., renv = TRUE, strict = FALSE) {
  argumentValues <- c(as.list(environment()), list(...)) # capture arguments for logging

  setWrapperActive("pucAggregate")

  extraArgs <- list(...)
  startinfo <- toolstartmessage(functionCallString("pucAggregate", argumentValues), "+")
  puc <- normalizePath(puc)
  if (file.exists(regionmapping)) regionmapping <- normalizePath(regionmapping)

  .aggregatePuc <- function(regionmapping, cfg, madratCfg, madratCodelabels, nestinglevel) {
    # need to use `::` because this is run in another R session
    if (file.exists("puc/renv.lock")) {
      renv::init()
      renv::restore(lockfile = "puc/renv.lock", prompt = FALSE)
    }
    withr::local_options(madrat_cfg = madratCfg,
                         madrat_codelabels = madratCodelabels,
                         gdt_nestinglevel = nestinglevel)
    madrat::localConfig(packages = "madrat", regionmapping = regionmapping,
                        forcecache = TRUE, .verbose = FALSE)
    if (!is.null(cfg$package)) withr::local_package(cfg$package)
    do.call(madrat::retrieveData, c(cfg$args, list(renv = FALSE)))
  }

  with_tempdir({
    untar(puc, exdir = "puc")
    cfg <- readRDS("puc/config.rds")
    if (!all(names(extraArgs) %in% cfg$pucArguments)) {
      stop("arguments provided that cannot be changed in the given puc! Allowed arguments are: ",
           paste(cfg$pucArguments, collapse = ", "))
    }
    cfg$args <- modifyList(cfg$args, extraArgs)
    cfg$args$cachetype <- "def"
    cfg$args$cachefolder <- "./puc"
    cfg$args$puc <- FALSE
    if (!is.null(strict)) cfg$args$strict <- strict
    if (isTRUE(renv)) {
      out <- capture.output(r(.aggregatePuc, list(regionmapping = regionmapping, cfg = cfg,
                                                  madratCfg = getOption("madrat_cfg"),
                                                  madratCodelabels = getOption("madrat_codelabels"),
                                                  nestinglevel = getOption("gdt_nestinglevel")),
                              spinner = FALSE, show = TRUE))
      message(paste(out, "\n"))
    } else {
      # only attach and detach if package is not already attached, might crash otherwise
      if (!is.null(cfg$package) && !cfg$package %in% .packages()) {
        withr::local_package(cfg$package)
      }
      localConfig(regionmapping = regionmapping, forcecache = TRUE, .verbose = FALSE)
      do.call(retrieveData, c(cfg$args, list(renv = FALSE)))
    }
  }, tmpdir = madTempDir())
  toolendmessage(startinfo)
}
