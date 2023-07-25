#' Tool: cacheGet
#'
#' Load fitting cache data (if available)
#'
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}.
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @return cached data, if cache is available, otherwise NULL
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' madrat:::cacheGet("calc", "TauTotal", packages = "madrat")
#' @importFrom digest digest
cacheGet <- function(prefix, type, args = NULL, graph = NULL, ...) {

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

  .isSet <- function(prefix, type, setting) {
    if (is.null(getConfig(setting))) return(FALSE)
    return(all(getConfig(setting) == TRUE) || any(c(type, paste0(prefix, type)) %in% getConfig(setting)))
  }

  if (.isSet(prefix, type, "ignorecache")) return(NULL)

  fname <- cacheName(prefix = prefix, type = type, args = args,  graph = graph, mode = "get", ...)

  if (is.null(fname)) return(NULL)

  stopifnot(isTRUE(getConfig("forcecache")) || !endsWith(fname, "FfingerprintError.rds"))

  vcat(1, " - loading cache ", basename(fname), fill = 300, show_prefix = FALSE)
  x <- try(readRDS(fname), silent = TRUE)
  if ("try-error" %in% class(x)) {
    vcat(0, " - corrupt cache file ", basename(fname), "! Continue without cache.")
    return(NULL)
  }
  if (is.list(x) && isTRUE(x$class %in% c("SpatRaster", "SpatVector"))) {
    for (elem in intersect(names(x), c("x", "weight"))) {
      x[[elem]] <- .terraLoad(x[[elem]])
    }
  }
  putMadratMessage(value = attr(x, "madratMessage"))
  attr(x, "id") <- fname
  return(x)
}
