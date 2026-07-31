#' Tool: cacheGet
#'
#' Load fitting cache data (if available)
#'
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @return cached data if available, otherwise NA
#' attr(, "id") will be set to the cache file name that should be written if the
#' data has to be recalculated.
#' attr(, "readFile") will be set to the cache file that was successfully read,
#' and is absent otherwise. This can differ from attr(, "id") if the data was
#' read from an rds file while another cache format is configured.
#'
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{cachePut}}, \code{\link{cacheNames}}
#' @keywords internal
#' @examples
#' madrat:::cacheGet("calc", "TauTotal")
cacheGet <- function(prefix, type, args = NULL) {

  .terraLoad <- function(x) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package `terra` required for caching of terra objects!")
    }

    if (inherits(x, c("PackedSpatRaster", "PackedSpatVector"))) {
      out <- terra::unwrap(x)
    } else {
      out <- terra::rast(x$file)
    }

    return(out)
  }

  x <- NA
  cacheFiles <- cacheNames(prefix = prefix, type = type, args = args)
  fname <- cacheFiles$write
  readFile <- cacheFiles$read

  if (isConfigSet(prefix, type, "ignorecache") || is.null(readFile)) {
    attr(x, "id") <- fname
    return(x)
  }

  stopifnot(file.exists(readFile))

  vcat(1, " - loading cache ", basename(readFile), fill = 300, show_prefix = FALSE)
  tryCatch({
    x <- cacheRead(readFile)
    # only set if reading actually succeeded, callers rely on this to detect a cache hit
    attr(x, "readFile") <- readFile
  }, error = function(e) {
    # the cache file may be corrupt, but the format may also just not be usable here
    vcat(0, " - could not read cache file ", basename(readFile), " (", conditionMessage(e),
         "). Will recalculate and write ", basename(fname), ".")
  })

  if (is.list(x) && isTRUE(x$class %in% c("SpatRaster", "SpatVector"))) {
    for (elem in intersect(names(x), c("x", "weight"))) {
      x[[elem]] <- .terraLoad(x[[elem]])
    }
  }
  putMadratMessage(value = attr(x, "madratMessage"))
  attr(x, "id") <- fname
  return(x)
}
