#' Tool: cacheName
#'
#' Load fitting cache data (if available)
#' @note \code{setConfig(forcecache=TRUE)} strongly affects the behavior
#' of \code{cacheName}. In read model it will also return cache names
#' with deviating hashes if no fitting cache file is found (in that case
#' it will just return the newest one). In write mode the hash in the name
#' will be left out since due to cache forcing it cannot be guaranteed
#' that the cache file agrees with the state represented by the hash.
#'
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}.
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param mode Context in which the function is used. Either "get" (loading) or
#' "put" (writing). In case of "put" the potential file name is returned.
#' When set to "get", a file name will only be returned if the file exists
#' (otherwise NULL) and in combination which \code{setConfig(forcecache=TRUE)}
#' even a cache file with deviating hash might get selected.
#' @param packages A character vector with packages for which the available
#' Sources/Calculations should be returned
#' @param globalenv	Boolean deciding whether sources/calculations in the global
#' environment should be included or not
#' @return Name of fitting cache file, if available, otherwise NULL
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' madrat:::cacheName("calc", "TauTotal")
#' @importFrom digest digest

cacheName <- function(prefix, type, args = NULL,  graph = NULL, mode = "put", packages = getConfig("packages"),
                      globalenv = getConfig("globalenv")) {
  fpprefix <- prefix
  if (prefix %in% c("convert", "correct")) fpprefix <- "read"

  fp <- fingerprint(name = paste0(fpprefix, type), graph = graph, details = (mode == "put"),
                    packages = packages, globalenv = globalenv)
  # as.character to strip attributes from fp
  if (identical(as.character(fp), "fingerprintError") && !isTRUE(getConfig("forcecache"))) {
    vcat(2, " - cacheName = NULL, because fingerprinting failed", show_prefix = FALSE)
    return(NULL)
  }
  call <- attr(fp, "call")
  if (prefix %in% c("convert", "correct")) {
    call <- c(call, sub(paste0(fpprefix, type), paste0(prefix, type), attr(fp, "call"), fixed = TRUE))
  }
  args <- try(cacheArgumentsHash(call, args))
  if ("try-error" %in% class(args)) return(NULL)

  .isSet <- function(prefix, type, setting) {
    return(all(getConfig(setting) == TRUE) || any(c(type, paste0(prefix, type)) %in% getConfig(setting)))
  }
  .fname <- function(prefix, type, fp, args) {
    return(paste0(getConfig("cachefolder"), "/", prefix, type, fp, args, ".rds"))
  }
  if (mode == "put" && !isFALSE(getConfig("forcecache"))) {
    # forcecache was at least partly active -> data consistency with
    # calculated hash is not guaranteed -> ignore hash
    return(.fname(prefix, type, "", args))
  }
  fname <- .fname(prefix, type, paste0("-F", fp), args)
  if (file.exists(fname) || mode == "put") return(fname)
  if (!.isSet(prefix, type, "forcecache")) {
    vcat(2, " - Cache file ", basename(fname), " does not exist", show_prefix = FALSE)
    return(NULL)
  }
  # no perfectly fitting file exists, try to find a similar one
  # (either with no fingerprint hash or with differing fingerprint)
  files <- Sys.glob(c(.fname(prefix, type, "-F*", args),
                      .fname(prefix, type, "", args)))

  # remove false positives
  if (is.null(args)) files <- files[!grepl("-[^F].*$", basename(files))]

  if (length(files) == 0) {
    vcat(2, " - No fitting cache file available", show_prefix = FALSE)
    vcat(3, " - Search pattern ", basename(.fname(prefix, type, "-F*", args)), show_prefix = FALSE)
    return(NULL)
  }
  if (length(files) == 1) {
    file <- files
  } else {
    file <- files[robustOrder(paste(file.mtime(files), basename(files)), decreasing = TRUE)][1]
  }
  if (!isWrapperActive("pucAggregate")) {
    vcat(1, " - forced cache does not match fingerprint ", fp,
         fill = 300, show_prefix = FALSE)
  }
  return(file)
}
