#' bundleCompile
#'
#' Function which takes a bundle tgz-file as create via \code{\link{retrieveData}}
#' and compiles the corresponding output with the given settings (in most cases
#' with a given region mapping)
#'
#'
#' @param bundle path to a bundle file
#' @param regionmapping region mapping to be used for aggregation. If not
#' specified the current default will be used.
#' @param ... (Optional) Settings that should be changed in addition. NOTE:
#' which settings can be modified varies from bundle to bundle and unsupported
#' choices can lead to errors.
#' @author Jan Philipp Dietrich
#' @seealso
#' \code{\link{retrieveData}},\code{\link{setConfig}}
#' @examples
#' \dontrun{
#' bundleCompile("rev1_bundle_example.tgz", regionmapping = "regionmappingH12.csv")
#' }
#' @importFrom withr with_tempdir
#' @importFrom utils untar modifyList
#' @export
bundleCompile <- function(bundle, regionmapping = getConfig("regionmapping"), ...) {
  argumentValues <- c(as.list(environment()), list(...)) # capture arguments for logging
  extraArgs <- list(...)
  startinfo <- toolstartmessage("bundleCompile", argumentValues, 0)

  setConfig(regionmapping = regionmapping, forcecache = TRUE, .local = FALSE)
  bundle <- normalizePath(bundle)

  with_tempdir({
    untar(bundle, exdir = "bundle")
    cfg <- readRDS("bundle/config.rds")
    if (!all(names(extraArgs) %in% cfg$bundleArguments)) {
      stop("arguments provided that cannot be changed in the given bundle!")
    }
    cfg$args <- modifyList(cfg$args, extraArgs)
    if (!is.null(cfg$package)) do.call("require", list(cfg$package))
    cfg$args$cachetype <- "def"
    cfg$args$cachefolder <- "./bundle"
    cfg$args$bundle <- FALSE
    do.call(retrieveData, cfg$args)
  })

  toolendmessage(startinfo)
}
