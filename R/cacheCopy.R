#' cacheCopy
#'
#' Copy cache files which were used for a given preprocessing.
#'
#' @param file Path to a log file or content of a log as character vector.
#' @param target Folder to which the files should be copied. If NULL no data is copied.
#' @param filter Regular expression to filter the cache files shown in the log file.
#' @return A vector of cache files which match the given log information and filter.
#' @author Jan Philipp Dietrich
#' @export
cacheCopy <- function(file, target=NULL, filter=NULL) {
  if (length(file) > 1 || any(grepl("\n", file))) {
    f <- unlist(strsplit(file, "\n"))
  } else {
    f <- readLines(file)
  }
  folder <- unique(sub('^.*\\"(.*)\\"', "\\1", grep("cachefolder", f, value = TRUE, perl = TRUE), perl = TRUE))
  if (length(folder) == 0) {
    warning("No information about cachefolder found, use folder as defined in getConfig('cachefolder')")
    folder <- getConfig("cachefolder")
  } else if (length(folder) > 1) {
    warning("More than one cache folder found. Use last mentioned folder")
    folder <- tail(folder, 1)
  }
  if (substring(folder, nchar(folder)) != "/") folder <- paste0(folder, "/")
  f <- grep("cache.*\\.rds", f, value = TRUE, perl = TRUE)
  files <- unique(sub("^.* ([^ ]*\\.rds).*$", "\\1", f))
  if (!is.null(filter)) files <- grep(filter, files, perl = TRUE, value = TRUE)
  files <- paste0(folder, files)
  if (!is.null(target)) {
    if (!dir.exists(target)) dir.create(target, recursive = TRUE)
    if (any(!file.exists(files))) {
      notfound <- files[!file.exists(files)]
      warning("Following cache files could not be found and are ignored: ", paste0(basename(notfound), collapse = ", "))
      files <- files[file.exists(files)]
    }
    if (length(files) > 0) file.copy(files, target)
  }
  return(files)
}
