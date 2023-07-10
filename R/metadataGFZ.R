#' metadataGFZ
#'
#' Function to extract metadata information of a data set hosted at GFZ dataservices
#' (https://dataservices.gfz-potsdam.de/portal/).
#'
#'
#' @param doi DOI of a data set hosted at GFZ dataservices
#' @return a list with entries "license", "citation", "authors" and "year"
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{toolstartmessage}}, \code{\link{vcat}}
#' @examples
#' \dontrun{
#' metadataGFZ("10.5880/pik.2019.004")
#' }
#' @export

metadataGFZ <- function(doi) {
  if (is.null(doi)) return(NULL)
  if (!grepl("10.5880", doi, fixed = TRUE)) stop("DOI does not belong to a GFZ dataservice entry")
  if (!grepl("http", doi)) doi <- paste0("http://doi.org/", doi)
  file <- tempfile()
  status <- try(download.file(doi, file, quiet = TRUE))
  if (inherits(status, "try-error")) {
    download.file(doi, file, method = "wget", extra = "--no-check-certificate", quiet = TRUE)
  }
  x <- readLines(file)
  unlink(file)
  o <- list()
  o$citation <- grep('class="citationtext"', x, fixed = TRUE, value = TRUE)
  if (length(o$citation) != 1) {
    warning("Cannot extract citation, return NULL")
    o$citation <- NULL
  } else {
    o$citation <- sub("^.*class=\"citationtext\">([^<]*).*$", "\\1", o$citation)
    o$authors <- strsplit(sub(" \\(.*$", "", o$citation), "; ")[[1]]
    .person <- function(x) {
      x <- strsplit(x, ", ")[[1]]
      return(person(x[2], x[1]))
    }
    o$authors <- do.call(c, lapply(o$authors, .person))
    o$year <- sub("^.*\\((.*)\\).*$", "\\1", o$citation)
  }
  findLicense <- grep("License:", x, fixed = TRUE)
  if (length(findLicense) != 1) {
    warning("Cannot extract license, return NULL")
    o$license <- NULL
  } else {
    o$license <- x[findLicense + 1]
    o$license <- sub("^[^>]*>([^<]*).*$", "\\1", o$license)
  }
  return(o)
}
