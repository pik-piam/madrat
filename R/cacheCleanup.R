#' cacheCleanup
#'
#' Delete cache files older than the specified number of days, based on atime/last access time file metadata. This
#' metadata is not always available and it is system dependent what atime/last access time actually means, so please
#' double check that the files to be deleted are detected properly.
#'
#' @param lifespanDays Files older than this many days are deleted.
#' @param cacheFolder Path to the folder to delete files from.
#' @param ask If TRUE (the default) the user is asked before deleting.
#' @param readlineFunction A function to get input from the user. Only intended for testing.
#' @export
cacheCleanup <- function(lifespanDays, cacheFolder, ask = TRUE, readlineFunction = readline) {
  stopifnot(length(lifespanDays) == 1, lifespanDays == as.integer(lifespanDays), lifespanDays >= 0,
            length(cacheFolder) == 1, dir.exists(cacheFolder),
            length(ask) == 1, ask %in% c(TRUE, FALSE))

  cacheFolder <- normalizePath(cacheFolder, winslash = "/")

  filesAccessTimes <- file.info(list.files(cacheFolder, full.names = TRUE), extra_cols = FALSE)["atime"]
  oldFiles <- rownames(filesAccessTimes)[filesAccessTimes[[1]] < Sys.time() - lifespanDays * 24 * 60 * 60]
  if (ask) {
    if (length(oldFiles) == 0) {
      message(paste0("No files older than ", lifespanDays, " days found."))
      return(invisible(NULL))
    }
    question <- paste("Are you sure you want to delete these", length(oldFiles), "files? (y/N) ")
    if (!tolower(readlineFunction(question)) %in% c("y", "yes")) {
      return(invisible(NULL))
    }
  }
  file.remove(oldFiles)
}
