#' cacheName
#'
#' Get the name of a cache file corresponding to the given args
#'
#' @note \code{setConfig(forcecache=TRUE)} strongly affects the behavior
#' of \code{cacheName}. If mode == "get" it will also return cache names
#' with deviating hashes if no fitting cache file is found (in that case
#' it will just return the newest one). If mode == "put" the fingerpring
#' (but not the argument hash) in the name
#' will be left out since due to cache forcing it cannot be guaranteed
#' that the cache file agrees with the state represented by the hash.
#'
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @param mode Context in which the function is used. Either "get" (loading) or
#' "put" (writing). In case of "put" the potential file name is returned.
#' When set to "get", a file name will only be returned if the file exists
#' (otherwise NULL). In combination with \code{setConfig(forcecache=TRUE)}
#' even a cache file with deviating hash might get selected.
#' @return cache file name, that file does not necessarily exist
#'
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' madrat:::cacheName("calc", "TauTotal", mode = "get")
cacheName <- function(prefix, type, args = NULL, mode = "put") { # TODO remove mode (only used in testing)
  fpprefix <- prefix
  if (prefix %in% c("convert", "correct")) {
    fpprefix <- "read"
  }

  fp <- fingerprint(name = paste0(fpprefix, type))

  call <- attr(fp, "call")
  if (prefix %in% c("convert", "correct")) {
    call <- c(call,
              sub(paste0(fpprefix, type),
                  paste0(prefix, type),
                  attr(fp, "call"),
                  fixed = TRUE))
  }
  argsHash <- cacheArgumentsHash(call, args,
                                 errorOnMismatch = !(prefix %in% c("read", "correct")))

  .fname <- function(prefix, type, fp, argsHash) {
    return(paste0(getConfig("cachefolder"), "/", prefix, type, fp, argsHash, ".rds"))
  }

  fname <- .fname(prefix, type, paste0("-F", fp), argsHash)
  fnameNoFingerprint <- .fname(prefix, type, "", argsHash)

  if (mode == "put" && !isFALSE(getConfig("forcecache"))) {
    # forcecache was at least partly active -> data consistency with
    # calculated fingerprint is not guaranteed -> ignore fingerprint
    return(fnameNoFingerprint)
  }
  if (file.exists(fname) || mode == "put") {
    return(fname)
  }
  if (!isConfigSet(prefix, type, "forcecache")) {
    vcat(2, " - Cache file ", basename(fname), " does not exist", show_prefix = FALSE)
    # returning fname even though that file does not exist -> calling function must
    # check whether the returned file exist
    return(fname)
  }

  if (isConfigSet(prefix, type, "ignorecache")) {
    # the returned path won't be read, but it's used for writing
    vcat(2, " - forcecache and ignorecache are both active", show_prefix = FALSE)
    return(fnameNoFingerprint)
  }

  # no perfectly fitting file exists, try to find a similar one for forcecache
  # (either with no fingerprint hash or with differing fingerprint)
  files <- Sys.glob(c(.fname(prefix, type, "-F*", argsHash),
                      fnameNoFingerprint))

  # remove false positives
  if (is.null(argsHash)) {
    files <- files[!grepl("-[^F].*$", basename(files))]
  }

  if (length(files) == 0) {
    vcat(2, " - No fitting cache file available", show_prefix = FALSE)
    vcat(3, " - Search pattern ", basename(.fname(prefix, type, "-F*", argsHash)), show_prefix = FALSE)
    return(fnameNoFingerprint)
  } else {
    # found one or more similar files, use the newest one
    fname <- files[robustOrder(paste(file.mtime(files), basename(files)), decreasing = TRUE)][1]
  }
  if (!isWrapperActive("pucAggregate")) {
    vcat(1, " - forced cache does not match fingerprint ", fp,
         fill = 300, show_prefix = FALSE)
  }
  return(fname)
}
