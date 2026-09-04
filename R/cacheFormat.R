#' registerCacheFormat
#'
#' Register a serialization format which can be used for madrat cache files via
#' \code{setConfig(cacheformat = ...)}. The formats "rds" (the default) and
#' "qs2" are always available, "qs2" requires the \code{qs2} package.
#'
#' Cache files are identified by their file extension, so each format must use a
#' distinct one. Extensions are restricted to alphanumeric characters.
#'
#' @param name Name of the format, e.g. "qs2".
#' @param write A function(x, file) writing object \code{x} to \code{file}.
#' @param read A function(file) returning the object stored in \code{file}.
#' @param extension File extension used for cache files of this format (without
#' leading dot). Defaults to \code{name}.
#' @param toRds Optional fast path conversion function(input, output) converting
#' a cache file of this format to a rds file. This is used when bundling puc files,
#' which always contain rds files.
#' @param packages Character vector of packages which must be installed for this format
#' to be usable, e.g. \code{"qs2"}. Checked by \code{\link{setConfig}} and at startup when
#' the format is selected, so that a missing package is reported immediately instead of
#' causing cache writes to silently fail later.
#' @return Invisibly, the registered format definition.
#' @author Patrick Rein
#' @seealso \code{\link{setConfig}}, \code{\link{cacheFormats}}
#' @family cache management
#' @examples
#' \dontrun{
#' registerCacheFormat("qs", write = qs::qsave, read = qs::qread, packages = "qs")
#' setConfig(cacheformat = "qs")
#' }
#' @export
registerCacheFormat <- function(name, write, read, extension = name, toRds = NULL, packages = NULL) {
  # a "-" would confuse the cache file name parsing in cacheNames
  # a "." would confuse the stem handling when converting to rds for puc files
  if (grepl("[^A-Za-z0-9]", extension)) {
    stop("Cache file extensions must only contain alphanumeric characters (got \"", extension, "\")")
  }

  if (!name %in% names(.cacheFormatRegistry())) {
    registeredExtensions <- unlist(lapply(.cacheFormatRegistry(), function(f) f$extension))
    if (extension %in% registeredExtensions) {
      stop("Extension \".", extension, "\" is already used by another cache format.")
    }
  }

  formats <- getOption("madrat_cacheformats", default = list())
  formats[[name]] <- list(extension = extension, write = write, read = read, toRds = toRds, packages = packages)
  options(madrat_cacheformats = formats) # nolint
  return(invisible(formats[[name]]))
}

#' @describeIn registerCacheFormat names of all currently registered cache formats
#' @export
cacheFormats <- function() {
  return(names(.cacheFormatRegistry()))
}

# formats shipped with madrat. These are defined here rather than registered in an
# .onLoad hook so that they are available without setup.
.builtinCacheFormats <- function() {
  rds <- list(extension = "rds",
              write = function(x, file) saveRDS(x, file = file, compress = getConfig("cachecompression")),
              read = function(file) readRDS(file),
              toRds = function(input, output) file.copy(input, output))
  qs2 <- list(extension = "qs2",
              write = function(x, file) qs2::qs_save(x, file = file),
              read = function(file) qs2::qs_read(file),
              toRds = function(input, output) qs2::qs_to_rds(input, output),
              packages = "qs2")
  return(list(rds = rds, qs2 = qs2))
}

# built-in formats, overwritten by and extended with formats registered via registerCacheFormat
.cacheFormatRegistry <- function() {
  formats <- .builtinCacheFormats()
  registered <- getOption("madrat_cacheformats")
  if (length(registered) > 0) formats[names(registered)] <- registered
  return(formats)
}

#' cacheFormat
#'
#' Look up a registered cache format definition.
#'
#' @param name Name of the format, defaults to the currently configured one.
#' @return The format definition, with the format name added as element "name".
#' @keywords internal
cacheFormat <- function(name = getConfig("cacheformat")) {
  formats <- .cacheFormatRegistry()
  if (!isTRUE(name %in% names(formats))) {
    stop("Unknown cache format \"", paste(name, collapse = ", "), "\". Available formats: ",
         paste0("\"", names(formats), "\"", collapse = ", "))
  }
  format <- formats[[name]]
  format$name <- name
  return(format)
}

#' @describeIn cacheFormat check that a format's required packages (see
#' \code{\link{registerCacheFormat}}) are installed. Must be called whenever a cache format is
#' selected (setConfig, initializeConfig), not from \code{cacheFormat} itself: that one is also
#' called from within \code{cacheWrite}, where an error would just be swallowed by
#' \code{cachePut}'s \code{tryCatch}, leaving the silent fallback to rds this check exists to
#' prevent.
#' @param hint Optional text appended to the error message, e.g. to point at the
#' environment variable which caused an unusable format to be selected.
#' @keywords internal
checkCacheFormatAvailable <- function(name, hint = NULL) {
  format <- cacheFormat(name) # errors on unknown names
  missing <- Filter(function(p) !requireNamespace(p, quietly = TRUE), format$packages)
  if (length(missing) > 0) {
    stop("Cache format \"", name, "\" requires the package(s) \"",
         paste(missing, collapse = "\", \""), "\", which are not installed.", hint)
  }
  return(invisible(format))
}

# file extensions to look for when searching a cache file, in order of preference:
# the configured format first, rds last as it is always readable (see cacheNames)
cacheExtensions <- function() {
  return(unique(c(cacheFormat()$extension, "rds")))
}

#' @importFrom tools file_ext
.cacheFormatByExtension <- function(extension) {
  formats <- .cacheFormatRegistry()
  fitting <- names(formats)[vapply(formats, function(f) identical(f$extension, extension), logical(1))]
  if (length(fitting) == 0) {
    stop("No cache format registered for file extension \".", extension, "\"")
  }
  return(cacheFormat(fitting[1]))
}

#' cacheRead / cacheWrite / cacheToRds
#'
#' Read, write and convert cache files using the format belonging to their file
#' extension. Dispatching on the extension rather than on the configured format
#' is what allows madrat to still read rds cache files while writing a different
#' format (see \code{\link{cacheNames}}).
#'
#' @param x Object to be written.
#' @param file Path of the cache file to be read/written, including file extension.
#' @param input Path of the cache file to be converted.
#' @param output Path of the rds file to be created.
#' @author Patrick Rein
#' @keywords internal
cacheRead <- function(file) {
  return(.cacheFormatByExtension(file_ext(file))$read(file))
}

#' @describeIn cacheRead write a cache file
#' @keywords internal
cacheWrite <- function(x, file) {
  # write to tempfile to avoid corrupt cache files in parallel running preprocessings
  tempfileName <- paste0(file, Sys.getenv("SLURM_JOB_ID", unset = ""))
  .cacheFormatByExtension(file_ext(file))$write(x, tempfileName)
  # file.rename reports failure via its return value instead of raising
  if (!file.rename(tempfileName, file)) {
    stop("could not rename ", tempfileName, " to ", file)
  }
  Sys.chmod(file, mode = "0666", use_umask = FALSE)
}

#' @describeIn cacheRead convert a cache file to rds
#' @keywords internal
cacheToRds <- function(input, output) {
  format <- .cacheFormatByExtension(file_ext(input))
  if (is.null(format$toRds)) {
    saveRDS(cacheRead(input), file = output)
  } else {
    format$toRds(input, output)
  }
  # return values of toRds functions are not standardized, so check the result instead
  return(file.exists(output))
}
