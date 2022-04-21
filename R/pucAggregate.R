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
#' within a renv environment (recommended) or not.
#' @author Jan Philipp Dietrich
#' @seealso
#' \code{\link{retrieveData}},\code{\link{setConfig}}
#' @examples
#' \dontrun{
#' pucAggregate("rev1_example.puc", regionmapping = "regionmappingH12.csv")
#' }
#' @importFrom withr with_tempdir local_package
#' @importFrom utils untar modifyList
#' @importFrom callr r
#' @importFrom renv activate restore
#' @export
pucAggregate <- function(puc, regionmapping = getConfig("regionmapping"), ..., renv = TRUE) {
  argumentValues <- c(as.list(environment()), list(...)) # capture arguments for logging
  extraArgs <- list(...)
  startinfo <- toolstartmessage("pucAggregate", argumentValues, "+")
  puc <- normalizePath(puc)
  if (file.exists(regionmapping)) regionmapping <- normalizePath(regionmapping)

  .aggregatePuc <- function(regionmapping, cfg, madratCfg, nestinglevel) {
    if (file.exists("puc/renv.lock")) {
      renv::activate()
      renv::restore(lockfile = "puc/renv.lock", prompt = FALSE)
    }
    withr::local_options(madrat_cfg = madratCfg, gdt_nestinglevel = nestinglevel)
    if (!is.null(cfg$package)) withr::local_package(cfg$package)
    madrat::setConfig(regionmapping = regionmapping, forcecache = TRUE,
                      .verbose = FALSE, .local = TRUE)
    do.call(retrieveData, c(cfg$args, list(renv = FALSE)))
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
    if (isTRUE(renv)) {
      out <- capture.output(r(.aggregatePuc, list(regionmapping = regionmapping, cfg = cfg,
                            madratCfg = getOption("madrat_cfg"),
                            nestinglevel = getOption("gdt_nestinglevel")),
                            spinner = FALSE, show = TRUE))
      message(paste(out, "\n"))
    } else {
      if (!is.null(cfg$package) && cfg$package != "madrat") withr::local_package(cfg$package)
      madrat::setConfig(regionmapping = regionmapping, forcecache = TRUE,
                        .verbose = FALSE, .local = TRUE)
      do.call(retrieveData, c(cfg$args, list(renv = FALSE)))
    }
  })

  toolendmessage(startinfo)
}
