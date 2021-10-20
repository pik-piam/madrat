#' cacheCleanup
#'
#' Delete cache files older than a specified period of time, based on atime file metadata.
#'
#' @param lifespanDays Files older than this many days are deleted.
#' @param cacheFolder Path to the folder to delete files from.
#' @param ask If TRUE (the default) the user is asked before deleting.
#' @param readlineFunction A function to get input from the user. Only intended for testing.
#' @export
cacheCleanup <- function(lifespanDays, cacheFolder, ask = TRUE, readlineFunction = readline) {
  if (!commandAvailable("find")) {
    stop("cacheCleanup requires the find command, but it is not available on your system.")
  }
  stopifnot(length(lifespanDays) == 1, lifespanDays == as.integer(lifespanDays), lifespanDays >= 0,
            length(cacheFolder) == 1, dir.exists(cacheFolder),
            length(ask) == 1, ask %in% c(TRUE, FALSE))

  findArgs <- c(shQuote(cacheFolder), "-atime", paste0("+", lifespanDays))
  oldFiles <- system2("find", findArgs, stdout = TRUE)
  if (ask) {
    if (length(oldFiles) == 0) {
      cat("No files older than ", lifespanDays, " days found in ", cacheFolder, ".")
      return(invisible(NULL))
    } else {
      question <- paste0("The following files are older than ", lifespanDays, " days:\n",
                         paste(oldFiles, collapse = "\n"), "\n",
                         "Are you sure you want to delete these files? (y/N) ")
      if (!tolower(readlineFunction(question)) %in% c("y", "yes")) {
        return(invisible(NULL))
      }
    }
  }
  system2("find", c(findArgs, "-delete"))
}
