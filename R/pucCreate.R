#' pucCreate
#'
#' Creates a puc-file ("portable unaggregated collection") for a collection that was already
#' computed via \code{\link{retrieveData}} (e.g. with \code{puc = FALSE}, or where a puc could
#' not be created at the time, for example due to strict mode). This makes it possible to
#' create a puc-file for an existing archive without having to rerun the underlying
#' calculations.
#'
#' @note This only works as long as the madrat cache files that were used to compute the
#' archive are still available on this machine, as the archive itself only contains the
#' aggregated collection, not the underlying cache files. If some of these cache files are
#' missing \code{pucCreate} will fail with an error naming them.
#'
#' @param archive path to a tgz-file as created by \code{\link{retrieveData}}
#' @param pucName (Optional) name (without the \code{.puc} extension) the puc-file should be
#' given. Only needed for archives created before \code{retrieveData} started recording this
#' name in \code{config.rds}.
#' @return Invisibly, the path to the created puc-file.
#' @author Patrick Rein
#' @seealso
#' \code{\link{retrieveData}},\code{\link{pucAggregate}}
#' @family aggregation
#' @examples
#' \dontrun{
#' pucCreate("rev1_h12_example.tgz")
#' }
#' @importFrom withr with_tempdir
#' @importFrom utils untar
#' @export
pucCreate <- function(archive, pucName = NULL) {
  argumentValues <- as.list(environment())
  startinfo <- toolstartmessage(functionCallString("pucCreate", argumentValues), "+")
  archive <- normalizePath(archive, mustWork = TRUE)

  with_tempdir({
    members <- untar(archive, list = TRUE)
    toExtract <- members[basename(members) %in% c(.pucFilesFileName, .pucExtraFiles)]
    if (!(.pucFilesFileName %in% basename(toExtract))) {
      stop("Archive does not contain a \"", .pucFilesFileName, "\" file. It does not look like ",
           "it was created by retrieveData, or it does not contain any cacheable data.")
    }
    untar(archive, files = toExtract, exdir = ".")

    cfg <- readRDS("config.rds")
    if (is.null(pucName)) {
      pucName <- cfg$pucName
      if (is.null(pucName)) {
        stop("Could not determine the puc name from config.rds. This archive was likely ",
             "created by a madrat version that did not yet store it there. ",
             "Please provide the \"pucName\" argument explicitly.")
      }
    }

    cacheFiles <- readLines(.pucFilesFileName)
    missingFiles <- cacheFiles[!file.exists(cacheFiles)]
    if (length(missingFiles) > 0) {
      stop("Cannot create puc file, the following cache files no longer exist:\n",
           paste(missingFiles, collapse = "\n"))
    }

    extraFiles <- normalizePath(Filter(file.exists, .pucExtraFiles))

    pucPath <- .createPuc(pucName = paste0(pucName, ".puc"), cacheFiles = cacheFiles,
                          extraFiles = extraFiles, requiredPackages = cfg$package)
  }, tmpdir = madTempDir())

  toolendmessage(startinfo, "-")
  return(invisible(pucPath))
}
