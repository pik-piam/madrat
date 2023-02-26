#' Tool: ManualDownload
#'
#' Support tool for the creation of download functions in cases where a fully automated data download is not
#' an option (e.g. due to a missing API). The function can be used to print a step-by-step guide for the user
#' how to manually retrieve the data and then asks for a (local) path where the data can be copied from.
#'
#' @param instructions Download instructions in form of a character vector describing how to manually
#' retrieve the data.
#' @param intro Introductory sentence to be shown first. Will not show up if set to NULL.
#' @param request A prompt which should show up after the instructions to ask for the local download location.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{downloadSource}}
#' @examples
#' \dontrun{
#' toolManualDownload(c("Log into website ABC",
#'                      "Download the data set XYZ"))
#' }
#' @importFrom withr local_connection
#' @export
toolManualDownload <- function(instructions, intro = "Data must be downloaded manually",
                               request = "Enter full path to the downloaded data:") {
  .getLine <- function() {
    if (interactive()) {
      # needed for e.g. RStudio and R in jupyter
      return(readline())
    }
    return(readLines(withr::local_connection(file("stdin")), n = 1))
  }
  msg <- function(...) vcat(1, ..., show_prefix = FALSE)
  if (!is.null(intro)) msg(intro)
  for(m in paste0(seq_along(instructions), ". ", instructions)) {
    msg(m)
  }
  msg(request)
  filePath <- .getLine()
  if (!file.exists(filePath)) stop("Data could not be found!")
  file.copy(filePath, ".")
  msg("Data has been succesfully copied. You can now delete ", normalizePath(filePath))
}
