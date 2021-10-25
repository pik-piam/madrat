#' findOldFiles
#'
#' Find files older than the specified number of days, based on file time metadata. This metadata is not available on
#' all systems and the semantics are also system dependent.
#'
#' @param daysThreshold Files older than this many days are returned.
#' @param path Path to where to look for old files.
#' @param timeType Which file metadata time should be used. One of atime (last access time), mtime (last modify time),
#' ctime (last metadata change).
#' @value A data.frame as returned by base::file.info, containing only files older than <daysThreshold> days.
#' @export
findOldFiles <- function(daysThreshold, path, timeType = c("atime", "mtime", "ctime")) {
  stopifnot(length(daysThreshold) == 1, is.numeric(daysThreshold), daysThreshold >= 0,
            length(path) == 1, dir.exists(path))
  timeType <- match.arg(timeType)

  filesAccessTimes <- file.info(list.files(normalizePath(path, winslash = "/"), full.names = TRUE))

  dateThreshold <- Sys.time() - daysThreshold * 24 * 60 * 60
  oldFiles <- filesAccessTimes[filesAccessTimes[[timeType]] < dateThreshold, ]
  return(oldFiles)
}
