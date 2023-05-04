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
    # write to tempfile to avoid corrupt cache files in parallel running preprocessings
    tempfileName <- paste0(fname, Sys.getenv("SLURM_JOB_ID", unset = ""))
    saveRDS(x, file = tempfileName, compress = getConfig("cachecompression"))
    file.rename(tempfileName, fname)
    Sys.chmod(fname, mode = "0666", use_umask = FALSE)
  }
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

  # write gpkg/tif to cache, then re-create SpatVector/SpatRaster referencing this new cache file
  if (inherits(x, "SpatVector")) {
    sourceFile <- paste0(file_path_sans_ext(fname), "-", name, ".gpkg")
    if (file.exists(sourceFile)) {
      message("Not writing ", sourceFile, " because it already exists.")
    } else {
      cat("copyTerraSources\n")
      print(system.time({
        copyTerraSources(x, name, fname)
      }))
      unlink(sourceFile)
      cat("writeVector\n")
      print(system.time({
        terra::writeVector(x, sourceFile)
      }))
    }
    out <- terra::vect(sourceFile)
    return(terra::wrap(out))
  } else if (inherits(x, "SpatRaster")) {
    sourceFile <- paste0(file_path_sans_ext(fname), "-", name, ".tif")
    if (file.exists(sourceFile)) {
      message("Not writing ", sourceFile, " because it already exists.")
    } else {
      cat("copyTerraSources\n")
      print(system.time({
        copyTerraSources(x, name, fname)
      }))
      unlink(sourceFile)
      cat("writeRaster\n")
      print(system.time({
        terra::writeRaster(x, sourceFile)
      }))
    }
    out <- terra::rast(sourceFile)
    return(terra::wrap(out, proxy = TRUE))
  } else {
    stop("madrat:::toolTerraToCache supports only SpatVector and SpatRaster")
  }
}

copyTerraSources <- function(x, name, fname) {
  sources <- terra::sources(x)
  if ("" %in% sources) {
    stop("file-based and in-memory parts in the same terra object can currently not be cached")
  }
  # the regex deals with sources such as 'NETCDF:"/PIK/inputdata/sources/LUH2v2h/states.nc":primf'
  sourceFiles <- unique(gsub('^[^"]*"|"[^"]*$', "", sources))
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
}
