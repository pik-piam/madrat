#' findOldFiles
#'
#' Find files older than the specified number of days, based on atime/last access time file metadata. This
#' metadata is not always available and it is system dependent what atime/last access time actually means.
#'
#' @param daysThreshold Files older than this many days are returned.
#' @param path Path to where to look for old files.
#' @value A data.frame as returned by base::file.info, containing only files older than <daysThreshold> days.
#' @export
findOldFiles <- function(daysThreshold, path) {
  stopifnot(length(daysThreshold) == 1, is.numeric(daysThreshold), daysThreshold >= 0,
            length(path) == 1, dir.exists(path))

  path <- normalizePath(path, winslash = "/")
  filesAccessTimes <- file.info(list.files(path, full.names = TRUE))

  dateThreshold <- Sys.time() - daysThreshold * 24 * 60 * 60
  oldFiles <- filesAccessTimes[filesAccessTimes[["atime"]] < dateThreshold, ]
  return(oldFiles)
}
