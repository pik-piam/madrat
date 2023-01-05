#' Tool: cachePut
#'
#' Save data to cache
#'
#' @param x data that should be written to cache
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' \dontrun{
#' example <- 1
#' madrat:::cachePut(example, "calc", "Example", packages = "madrat")
#' }
#' @importFrom digest digest

cachePut <- function(x, prefix, type, args = NULL, graph = NULL, ...) {

  .spatRaster2Cache <- function(x, name, fname) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("Package `terra` required for caching of SpatRaster objects!")
    if (length(terra::sources(x)) > 1) {
      stop("Multiple sources of SpatRaster objects in caching currently not supported!")
    }
    if (terra::inMemory(x)) return(x)
    sourceName <- terra::sources(x)
    sourceType <- tools::file_ext(sourceName)
    targetName <- sub("\\..*$", paste0("-", name, ".", sourceType), fname)
    file.copy(sourceName, targetName)
    Sys.chmod(targetName, mode = "0666", use_umask = FALSE)
    return(list(class = "SpatRaster",
                names = names(x),
                file = targetName))
  }

  if (is.list(x) && isFALSE(x$cache)) {
    vcat(1, " - cache disabled for ", prefix, type, fill = 300, show_prefix = FALSE)
    return()
  }

  fname <- cacheName(prefix = prefix, type = type, args = args,  graph = graph, mode = "put", ...)
  if (!is.null(fname)) {
    if (!dir.exists(dirname(fname))) {
      dir.create(dirname(fname), recursive = TRUE)
    }
    attr(x, "cachefile") <- basename(fname)
    vcat(1, " - writing cache ", basename(fname), fill = 300, show_prefix = FALSE)
    if (is.list(x)) {
      for (elem in c("x", "weight")) {
        if (inherits(x[[elem]], "SpatRaster")) {
          x[[elem]] <- .spatRaster2Cache(x[[elem]], elem, fname)
        }
      }
    }
    saveRDS(x, file = fname, compress = getConfig("cachecompression"))
    Sys.chmod(fname, mode = "0666", use_umask = FALSE)
  }
}
