#' downloadSource
#'
#' Download a source. The function is a wrapper for specific functions designed
#' for the different possible source types.
#'
#'
#' @param type source type, e.g. "IEA". A list of all available source types
#' can be retrieved with function \code{\link{getSources}("download")}.
#' @param subtype For some sources there are subtypes of the source, for these
#' source the subtype can be specified with this argument. If a source does not
#' have subtypes, subtypes should not be set.
#' @param overwrite Boolean deciding whether existing data should be
#' overwritten or not.
#' @note The underlying download-functions are required to provide a list of information back to
#' \code{downloadSource}. Following list entries should be provided:
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
#' @importFrom yaml write_yaml
#' @importFrom withr local_dir
#' @author Jan Philipp Dietrich, David Klein
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- downloadSource("Tau", subtype = "historical")
#' }
#'
#' @export
downloadSource <- function(type, subtype = NULL, overwrite = FALSE) {
  argumentValues <- as.list(environment())  # capture arguments for logging
  startinfo <- toolstartmessage("downloadSource", argumentValues, "+")
  on.exit(toolendmessage(startinfo, "-"))

  # check type input
  if (!all(is.character(type)) || length(type) != 1) stop("Invalid type (must be a single character string)!")
  if (!is.null(subtype) && (!all(is.character(subtype)) || length(subtype) != 1)) {
    stop("Invalid subtype (must be a single character string)!")
  }

  functionname <- prepFunctionName(type = type,
                                   prefix = "download",
                                   ignore = ifelse(is.null(subtype), "subtype", NA))

  if (!grepl("subtype=subtype", functionname, fixed = TRUE)) subtype <- NULL

  if (!file.exists(getConfig("sourcefolder"))) dir.create(getConfig("sourcefolder"), recursive = TRUE)

  typesubtype <- paste(c(make.names(type), make.names(subtype)), collapse = "/")

  local_dir(getConfig("sourcefolder"))
  if (file.exists(typesubtype)) {
    if (overwrite) {
      unlink(typesubtype, recursive = TRUE)
    } else {
      stop("Source folder for source \"", typesubtype,
           "\" does already exist! Delete folder or activate overwrite to proceed!")
    }
  }

  dir.create(typesubtype, recursive = TRUE)
  absolutePathTypesubtype <- normalizePath(typesubtype)
  local_dir(absolutePathTypesubtype)
  on.exit({
    if (length(dir(absolutePathTypesubtype)) == 0) {
      unlink(absolutePathTypesubtype, recursive = TRUE)
    }
  }, add = TRUE, after = FALSE)
  meta <- eval(parse(text = functionname))

  # define mandatory elements of meta data and check if they exist
  mandatory <- c("url", "author", "title", "license", "description", "unit")
  if (!all(mandatory %in% names(meta))) {
    vcat(0, "Missing entries in the meta data of function '", functionname[1], "': ",
         toString(mandatory[!mandatory %in% names(meta)]))
  }

  # define reserved elements of meta data and check if they already exist
  reserved <- c("call", "accessibility")
  if (any(reserved %in% names(meta))) {
    vcat(0, "The following entries in the meta data of the function '", functionname[1],
         "' are reserved and will be overwritten: ", reserved[reserved %in% names(meta)])
  }

  # set reserved meta data elements
  meta$call <- list(origin  = paste0(gsub("\\s{2,}", " ", paste(deparse(match.call()), collapse = "")),
                                     " -> ", functionname, " (madrat ", unname(getNamespaceVersion("madrat")),
                                     " | ", attr(functionname, "pkgcomment"), ")"),
                    type    = type,
                    subtype = ifelse(is.null(subtype), "none", subtype),
                    time    = format(Sys.time(), "%F %T %Z"))
  meta$accessibility <- ifelse(!is.null(meta$doi), "gold", "silver")

  # reorder meta entries
  preferredOrder <- c("title", "description", "author", "doi", "url", "accessibility", "license", "version",
                      "release_date", "unit", "call", "reference")
  order <- c(intersect(preferredOrder, names(meta)), setdiff(names(meta), preferredOrder))

  write_yaml(meta[order], "DOWNLOAD.yml")
}
