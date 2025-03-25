#' Tool: cacheGet
#'
#' Load fitting cache data (if available)
#'
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @return cached data if available, otherwise NA
#' attr(, "id") will be set to the cache file name that was (tried to be) loaded
#'
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
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
  fname <- cacheName(prefix = prefix, type = type, args = args, mode = "get")

  if (isConfigSet(prefix, type, "ignorecache") || !file.exists(fname)) {
    attr(x, "id") <- fname
    return(x)
  }

  stopifnot(file.exists(fname))

  vcat(1, " - loading cache ", basename(fname), fill = 300, show_prefix = FALSE)
  tryCatch({
    x <- readRDS(fname)
  }, error = function(e) {
    vcat(0, " - corrupt cache file ", basename(fname),
         ". Will recalculate and overwrite corrupt cache file.")
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
