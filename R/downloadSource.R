#' downloadSource
#'
#' Download a source. The function is a wrapper for specific functions designed
#' for the different possible source types.
#'
#' @param type source type, e.g. "IEA". A list of all available source types can be retrieved with
#' function \code{\link{getSources}("download")}.
#' @param subtype For some sources there are subtypes of the source, for these source the subtype can be specified
#' with this argument. If a source does not have subtypes, subtypes should not be set.
#' @param overwrite Boolean deciding whether existing data should be overwritten or not.
#' @param numberOfTries Integer determining how often readSource will check whether a running download is finished
#' before exiting with an error. Between checks readSource will wait 30 seconds. Has no effect if the sources that
#' should be read are not currently being downloaded.
#' @note The underlying download-functions are required to provide a list of information
#' back to \code{downloadSource}. Following list entries should be provided:
#' \itemize{
#' \item \bold{url} - full path to the file that should be downloaded
#' \item \bold{title} - title of the data source
#' \item \bold{author} - author(s) of the data set
#' \item \bold{license} - license of the data set. Put \bold{unknown} if not specified.
#' \item \bold{description} - description of the data source
#' \item \bold{unit} - unit(s) of the data
#' \item \bold{doi} (optional) - a DOI URL to the data source
#' \item \bold{version} (optional) - version number of the data set
#' \item \bold{release_date} (optional) - release date of the data set
#' \item \bold{reference} (optional) - A reference for the data set (e.g. a paper, if the data was derived from it)
#' }
#' This user-provided data is enriched by automatically derived metadata:
#' \itemize{
#' \item \bold{call} - Information about the used madrat function call to download the data
#' will check whether there are any values below the given threshold and warn in this case
#' \item \bold{accessibility} - A measure of quality for the accessibility of the data. Currently it distinguished
#' between \bold{iron} (manual access), \bold{silver} (automatic access via URL) and \bold{gold} (automatic access via
#' DOI).
#' }
#' Besides the names above (user-provided and automatically derived) it is possible to add custom metadata entries by
#' extending the return list with additional, named entries.
#' @author Jan Philipp Dietrich, David Klein, Pascal Sauer
#' @examples
#' \dontrun{
#' a <- downloadSource("Tau", subtype = "historical")
#' }
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}}
#' @importFrom yaml write_yaml
#' @importFrom withr local_dir defer with_dir
#' @export
downloadSource <- function(type, subtype = NULL, overwrite = FALSE, numberOfTries = 300) { # nolint: cyclocomp_linter.
  argumentValues <- as.list(environment())  # capture arguments for logging

  setWrapperActive("downloadSource")
  setWrapperInactive("wrapperChecks")

  startinfo <- toolstartmessage(functionCallString("downloadSource", argumentValues), "+")
  defer({
    toolendmessage(startinfo, "-")
  })
  stopifnot(as.integer(numberOfTries) == numberOfTries, length(numberOfTries) == 1, numberOfTries >= 1)

  # check type input
  if (!all(is.character(type)) || length(type) != 1) {
    stop("Invalid type (must be a single character string)!")
  }
  if (!is.null(subtype) && (!all(is.character(subtype)) || length(subtype) != 1)) {
    stop("Invalid subtype (must be a single character string)!")
  }

  functionCall <- prepFunctionName(type = type, prefix = "download")
  functionName <- sub("\\(.*$", "", functionCall)
  functionFormals <- formals(eval(parse(text = functionName)))

  if (is.null(subtype)) {
    # get default subtype argument if available, otherwise NULL
    subtype <- functionFormals[["subtype"]]
  } else if (!"subtype" %in% names(functionFormals)) {
    stop(functionName, "does not have a subtype argument, but subtype '", subtype, "' was provided.")
  }

  functionCall <- prepFunctionName(type = type, prefix = "download", ignore = if (is.null(subtype)) "subtype" else NA)

  if (!file.exists(getConfig("sourcefolder"))) {
    dir.create(getConfig("sourcefolder"), recursive = TRUE)
  }

  typesubtype <- paste(c(make.names(type), make.names(subtype)), collapse = "/")
  downloadInProgressDirectory <- paste0(typesubtype, "-downloadInProgress")

  local_dir(getConfig("sourcefolder"))
  if (dir.exists(typesubtype)) {
    if (dir.exists(downloadInProgressDirectory)) {
      warning("The source folders ", typesubtype, " and ", downloadInProgressDirectory, " should not exist at the ",
              "same time. Please delete ", normalizePath(downloadInProgressDirectory, winslash = "/"))
    }
    if (overwrite) {
      unlink(typesubtype, recursive = TRUE)
    } else {
      stop('Source folder for source "', typesubtype, '" does already exist. Delete that folder or ',
           "call downloadSource(..., overwrite = TRUE) if you want to re-download.",
           if (is.null(subtype)) " Note: subtype is NULL, is that intended?")
    }
  } else if (dir.exists(downloadInProgressDirectory)) { # the download is already running in another R session
    for (i in seq_len(numberOfTries - 1)) { # -1 because one try was already done before
      argsString <- paste(list(list(type = type, subtype = subtype))) # use paste + list for nicer string output
      argsString <- substr(argsString, 6, nchar(argsString) - 1) # remove superfluous list from string
      cat("downloadSource(", argsString, ") is already in progress, waiting 30 seconds...")
      Sys.sleep(30)
      if (dir.exists(typesubtype)) {
        # the parallel running download finished, nothing to do here
        return()
      }
    }
    if (dir.exists(downloadInProgressDirectory)) {
      stop("The download did not finish in time. If the download is no longer running delete ",
           normalizePath(downloadInProgressDirectory, winslash = "/"))
    } else {
      warning("Download was not finished, but is no longer running. Starting new download.")
    }
  }

  dir.create(downloadInProgressDirectory, recursive = TRUE)
  absolutePath <- normalizePath(downloadInProgressDirectory)
  defer({
    unlink(absolutePath, recursive = TRUE)
  })
  with_dir(downloadInProgressDirectory, {
    setWrapperActive("wrapperChecks")
    meta <- withMadratLogging(eval(parse(text = functionCall)))
    setWrapperInactive("wrapperChecks")

    # define mandatory elements of meta data and check if they exist
    mandatory <- c("url", "author", "title", "license", "description", "unit")
    if (!all(mandatory %in% names(meta))) {
      vcat(0, "Missing entries in the meta data of function '", functionCall[1], "': ",
           toString(mandatory[!mandatory %in% names(meta)]))
    }

    # define reserved elements of meta data and check if they already exist
    reserved <- c("call", "accessibility")
    if (any(reserved %in% names(meta))) {
      vcat(0, "The following entries in the meta data of the function '", functionCall[1],
           "' are reserved and will be overwritten: ", reserved[reserved %in% names(meta)])
    }

    # set reserved meta data elements
    meta$call <- list(origin  = paste0(gsub("\\s{2,}", " ", paste(deparse(match.call()), collapse = "")),
                                       " -> ", functionCall, " (madrat ", unname(getNamespaceVersion("madrat")),
                                       " | ", attr(functionCall, "pkgcomment"), ")"),
                      type    = type,
                      subtype = ifelse(is.null(subtype), "none", subtype),
                      time    = format(Sys.time(), "%F %T %Z"))
    meta$accessibility <- ifelse(!is.null(meta$doi), "gold", "silver")

    # reorder meta entries
    preferredOrder <- c("title", "description", "author", "doi", "url", "accessibility", "license", "version",
                        "release_date", "unit", "call", "reference")
    order <- c(intersect(preferredOrder, names(meta)), setdiff(names(meta), preferredOrder))

    write_yaml(meta[order], "DOWNLOAD.yml")
  })

  file.rename(downloadInProgressDirectory, typesubtype)
}
