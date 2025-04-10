#' cacheName
#'
#' Get the name of a cache file corresponding to the given args
#'
#' @note With \code{setConfig(forcecache=TRUE)} cacheName will also return
#' cache file names with deviating fingerprint if no fitting cache file is found
#' (if there are multiple it will just return the newest one).
#'
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @return absolute path to a cache file that does not necessarily exist
#'
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{cachePut}}
#' @examples
#' madrat:::cacheName("calc", "TauTotal")
cacheName <- function(prefix, type, args = NULL) {
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

  if (file.exists(fname)) {
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
