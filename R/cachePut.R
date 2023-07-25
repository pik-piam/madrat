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
  tryCatch({
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
          if (inherits(x[[elem]], c("SpatRaster", "SpatVector"))) {
            x[[elem]] <- toolTerraToCache(x[[elem]], elem, fname)
          }
        }
      }

      attr(x, "madratMessage") <- getMadratMessage(fname = paste0(prefix, type))

      # write to tempfile to avoid corrupt cache files in parallel running preprocessings
      tempfileName <- paste0(fname, Sys.getenv("SLURM_JOB_ID", unset = ""))
      saveRDS(x, file = tempfileName, compress = getConfig("cachecompression"))
      file.rename(tempfileName, fname)
      Sys.chmod(fname, mode = "0666", use_umask = FALSE)
    }
  }, error = function(e) {
    vcat(0, " - could not write cache file: ", e$message, fill = 300, show_prefix = FALSE)
  })
}

# madrat is confused when using tools::, maybe thinks this has to do with tool functions, so need to import
#' @importFrom tools file_path_sans_ext file_ext
toolTerraToCache <- function(x, name, fname) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package `terra` required for caching terra objects!")
  }
  if (all(terra::sources(x) == "")) {
    return(terra::wrap(x))
  }

  sources <- terra::sources(x)
  if ("" %in% sources) {
    stop("file-based and in-memory parts in the same terra object can currently not be cached")
  }
  # the regex deals with sources such as 'NETCDF:"/PIK/inputdata/sources/LUH2v2h/states.nc":primf'
  sourceFiles <- unique(gsub('^[^"]*"|"[^"]*$', "", sources))
  # copy all source files into the cache
  for (sourceFile in sourceFiles) {
    targetName <- paste0(file_path_sans_ext(fname), "-", name, ".", file_ext(sourceFile))
    sources <- sub(sourceFile, targetName, sources, fixed = TRUE)
    file.copy(sourceFile, targetName)
    Sys.chmod(targetName, mode = "0666", use_umask = FALSE)

    if (file_ext(sourceFile) == "shp") {
      # copy all shapefile auxiliary files, see https://en.wikipedia.org/wiki/Shapefile
      for (ext in c("shx", "dbf", "prj", "sbn", "sbx", "fbn", "fbx", "ain",
                    "aih", "ixs", "mxs", "atx", "shp.xml", "cpg", "qix")) {
        auxSource <- paste0(file_path_sans_ext(sourceFile), ".", ext)
        if (file.exists(auxSource)) {
          auxTarget <- paste0(file_path_sans_ext(targetName), ".", ext)
          file.copy(auxSource, auxTarget)
          Sys.chmod(auxTarget, mode = "0666", use_umask = FALSE)
        }
      }
    }
  }
  stopifnot(identical(intersect(sources, terra::sources(x)), character(0)))

  # re-create x using sources copied to the cache
  if (inherits(x, "SpatVector")) {
    x2 <- terra::vect(sources)
  } else if (inherits(x, "SpatRaster")) {
    x2 <- terra::rast(sources)
    if (length(terra::units(x2)) == length(terra::units(x))) {
      terra::units(x2) <- terra::units(x)
    }
    if (length(terra::time(x2)) == length(terra::time(x))) {
      terra::time(x2) <- terra::time(x)
    }
  } else {
    stop("Expected x to be SpatVector or SpatRaster")
  }

  if (length(names(x2)) == length(names(x))) {
    names(x2) <- names(x)
  } else {
    stop("Cannot cache this terra object, because loading it from cache would yield a different number of layers. ",
         "Add `cache = FALSE` to the returned list to disable caching.")
  }

  return(terra::wrap(x2))
}
