#' cacheCleanup
#'
#' Delete files older than the specified number of days, based on file time metadata (per default atime = last access
#' time).
#'
#' File time metadata is not available on all systems and the semantics are also system dependent, so please be
#' careful and check that the correct files are deleted. This function will return a data.frame containing all
#' files that would be deleted if the user answers 'n' to the question. If deleting files fails a warning is created.
#'
#' @param daysThreshold Files older than this many days are deleted/returned.
#' @param path Path to where to look for old files.
#' @param timeType Which file metadata time should be used. One of atime (last access time, default),
#' mtime (last modify time), ctime (last metadata change).
#' @param ask Whether to ask before deleting.
#' @param readlineFunction Only needed for testing. A function to prompt the user for input.
#' @return If the user answers 'n', a data.frame as returned by base::file.info, containing only files older than
#' <daysThreshold> days.
#' @importFrom withr local_dir
#' @export
cacheCleanup <- function(daysThreshold, path = getConfig("cachefolder", verbose = FALSE),
                         timeType = c("atime", "mtime", "ctime"), ask = TRUE, readlineFunction = readline) {
  timeType <- match.arg(timeType)
  stopifnot(length(daysThreshold) == 1, is.numeric(daysThreshold), daysThreshold >= 0,
            length(path) == 1, dir.exists(path),
            length(ask) == 1, ask %in% c(TRUE, FALSE))

  path <- normalizePath(path, winslash = "/")

  if (ask && !tolower(readlineFunction(paste("Is the path correct?", path, "(y/N) "))) %in% c("y", "yes")) {
    stop("Please pass the correct path.")
  }
  local_dir(path)

  message("Loading file timestamps...")
  filesAccessTimes <- file.info(list.files())
  dateThreshold <- Sys.time() - daysThreshold * 24 * 60 * 60
  oldFiles <- filesAccessTimes[filesAccessTimes[[timeType]] < dateThreshold, ]

  if (ask) {
    if (!requireNamespace("testthat", quietly = TRUE) || !testthat::is_testing()) { # do not print when testing
      print(oldFiles)
    }
    if (!tolower(readlineFunction("Do you want to delete these files? (y/N) ")) %in% c("y", "yes")) {
      return(oldFiles)
    }
  }

  notDeleted <- rownames(oldFiles)[!file.remove(rownames(oldFiles))]
  if (length(notDeleted) > 0) {
    warning("Could not delete the following files: ", paste(notDeleted, collapse = ", "))
  }
}
