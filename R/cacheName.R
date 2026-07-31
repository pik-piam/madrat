#' cacheName
#'
#' Get the name of a cache file corresponding to the given args
#'
#' @note With \code{setConfig(forcecache=TRUE)} cacheName will also look for
#' cache files with deviating fingerprint if no fitting cache file is found
#' (if there are multiple it will just use the newest one).
#'
#' Cache files are searched for in the configured cache format (see
#' \code{\link{setConfig}}) first and in "rds" second. This way an existing rds
#' cache stays usable after switching to another cache format. Files found via
#' this fallback are only read, never rewritten, so the returned write target
#' always uses the configured format.
#'
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @return absolute path to the cache file that should be written for the given
#' arguments (in the configured cache format). This file does not necessarily
#' exist. \code{attr(, "readFile")} contains the path of an already existing,
#' fitting cache file which should be read (possibly in another format), or NULL
#' if there is none.
#'
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{cachePut}}
#' @keywords internal
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

  # vectorized over extension
  .fname <- function(fp, argsHash, extension) {
    return(paste0(getConfig("cachefolder"), "/", prefix, type, fp, argsHash, ".", extension))
  }

  extensions <- cacheExtensions()
  forcecache <- isConfigSet(prefix, type, "forcecache")

  # The file to write depends only on forcecache and always uses the configured format.
  writeName <- .fname(if (forcecache) "" else paste0("-F", fp), argsHash, extensions[1])

  # The file to read may be in any registered format, preferring the configured one.
  readName <- NULL
  fittingFiles <- .fname(paste0("-F", fp), argsHash, extensions)
  if (any(file.exists(fittingFiles))) {
    # identical fingerprint means identical content, so prefer the format we read fastest
    readName <- fittingFiles[file.exists(fittingFiles)][1]
  } else if (!forcecache) {
    vcat(2, " - Cache file ", basename(fittingFiles[1]), " does not exist", show_prefix = FALSE)
  } else if (isConfigSet(prefix, type, "ignorecache")) {
    vcat(2, " - forcecache and ignorecache are both active", show_prefix = FALSE)
  } else {
    # no perfectly fitting file exists, try to find a similar one for forcecache
    # (either with no fingerprint hash or with differing fingerprint)
    files <- Sys.glob(c(.fname("-F*", argsHash, extensions),
                        .fname("", argsHash, extensions)))

    # remove false positives
    if (is.null(argsHash)) {
      files <- files[!grepl("-[^F].*$", basename(files))]
    }

    if (length(files) == 0) {
      vcat(2, " - No fitting cache file available", show_prefix = FALSE)
      vcat(3, " - Search pattern ", paste(basename(.fname("-F*", argsHash, extensions)), collapse = ", "),
           show_prefix = FALSE)
    } else {
      # found one or more similar files, use the newest one
      readName <- files[robustOrder(paste(file.mtime(files), basename(files)), decreasing = TRUE)][1]
      if (!isWrapperActive("pucAggregate")) {
        vcat(1, " - forced cache does not match fingerprint ", fp,
             fill = 300, show_prefix = FALSE)
      }
    }
  }

  attr(writeName, "readFile") <- readName
  return(writeName)
}
