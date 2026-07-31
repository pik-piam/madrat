#' cacheNames
#'
#' Get the names of the cache files corresponding to the given args
#'
#' @note With \code{setConfig(forcecache=TRUE)} cacheNames will also look for
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
#' @return a list with two elements: \code{write}, the absolute path to the cache
#' file that should be written for the given arguments (in the configured cache
#' format), which does not necessarily exist, and \code{read}, the absolute path
#' of an already existing, fitting cache file which should be read (possibly in
#' another format), or NULL if there is none.
#'
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{cachePut}}
#' @keywords internal
#' @examples
#' madrat:::cacheNames("calc", "TauTotal")
cacheNames <- function(prefix, type, args = NULL) {
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

  extensions <- cacheExtensions() # Contains configured extension as well as rds
  forcecache <- isConfigSet(prefix, type, "forcecache")

  # The file to write depends only on forcecache and always uses the configured format.
  writeName <- .fname(if (forcecache) "" else paste0("-F", fp), argsHash, extensions[1])

  # The file to read may be in any registered format, preferring the configured one.
  readName <- NULL
  fittingFiles <- .fname(paste0("-F", fp), argsHash, extensions)
  existingFiles <- fittingFiles[file.exists(fittingFiles)]
  if (length(existingFiles) > 0) {
    # identical fingerprint means identical content, so prefer the configured format
    readName <- existingFiles[1]
  } else if (!forcecache) {
    vcat(2, " - Cache file ", basename(fittingFiles[1]), " does not exist", show_prefix = FALSE)
  } else if (isConfigSet(prefix, type, "ignorecache")) {
    vcat(2, " - forcecache and ignorecache are both active", show_prefix = FALSE)
  } else { # forceCache is TRUE
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

  return(list(write = writeName, read = readName))
}
