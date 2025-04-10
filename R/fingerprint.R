#' fingerprint
#'
#' Function which creates a unique fingerprint for a madrat function based on
#' the code of the function itself, other madrat functions which are called by
#' this function and of all source folders involved in the process.
#' The fingerprint can serve as an indication whether the workflow for the given
#' function has been most likely changed, or not. If all involved source folders
#' and the code of all involved functions remains the same, also the fingerprint
#' will stay the same, otherwise it will change. Hence, it can be to figure out
#' whether a cache file can be used for further calculations, or whether the
#' calculation should be redone.
#'
#' @note For a better performance only the first 300 bytes of each file and the
#' corresponding file size is hashed.
#' As the fingerprint function only takes madrat-based functions into account
#' (e.g. read-functions or calc-functions), but does ignore all other functions
#' there might be cases where calculations actually changed, but the fingerprint
#' is still the same. In a similar fashion it is possible that the fingerprint
#' changes even though the workflow stayed the same
#' (as the dependencies are sometimes overestimated).
#'
#' @param name Name of the function to be analyzed
#' @return A fingerprint (hash) of all provided sources
#'
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{readSource}}
#' @examples
#' madrat:::fingerprint("toolGetMapping")
fingerprint <- function(name) {
  dependencies <- getDependencies(name, direction = "in", self = TRUE, packages = getConfig("packages"))

  fingerprintFunctions <- dependencies$hash[robustOrder(dependencies$call)]
  names(fingerprintFunctions) <- dependencies$call[robustOrder(dependencies$call)]

  # handle special requests via flags
  .tmp <- function(x) {
    return(robustSort(sub(":+", ":::", x)))
  }
  ignore <- .tmp(attr(dependencies, "flags")$ignore)
  monitor <- .tmp(attr(dependencies, "flags")$monitor)
  # if conflicting information is giving (monitor and ignore at the same time,
  # prioritize monitor request)
  ignore <- setdiff(ignore, monitor)
  # add calls from the monitor list which are not already monitored
  fingerprintMonitored <- fingerprintCall(setdiff(monitor, names(fingerprintFunctions)))
  # ignore functions mentioned in the ignore list
  fingerprintFunctions <- fingerprintFunctions[setdiff(names(fingerprintFunctions), ignore)]

  sources <- substring(dependencies$func[dependencies$type == "read"], 5)
  sources <- robustSort(sources)
  sources <- vapply(sources, function(t) getSourceFolder(type = t, subtype = NULL), character(1))
  fingerprintSources <- fingerprintFiles(sources)

  fingerprintMappings <- fingerprintFiles(attr(dependencies, "mappings"))
  fingerprint <- c(fingerprintFunctions, fingerprintSources, fingerprintMappings, fingerprintMonitored)
  fingerprint <- fingerprint[robustOrder(basename(names(fingerprint)))]

  # Cache files became incompatible when readSource was allowed to return non-magpie objects.
  # Referring to madrat versions before this change as "old" and after this change as "new" here:
  # Hashing the string "v2" leads to completely new hashes, and thus cache files with different names.
  # Old madrat versions will never read/write these new cache files, and new madrat versions will never
  # read/write cache files created with an old madrat version.
  result <- digest::digest(list("v2", unname(fingerprint)), algo = getConfig("hash"))

  if (getConfig("verbosity") >= 3) {
    attr(result, "details") <- fingerprint # for testing
    vcat(3, "hash components (", result, "):", show_prefix = FALSE)
    for (n in names(fingerprint)) {
      vcat(3, "  ", fingerprint[n], " | ", basename(n), " | ", n, show_prefix = FALSE)
    }
  }
  attr(result, "call") <- dependencies$call[dependencies$func == name]
  return(result)
}

fingerprintCall <- function(name) {
  .tmp <- function(x) {
    f <- try(eval(parse(text = x)), silent = TRUE)
    if ("try-error" %in% class(f)) {
      return(NULL)
    }
    return(digest::digest(paste(deparse(f), collapse = " "), algo = getConfig("hash")))
  }
  return(unlist(sapply(name, .tmp))) # nolint
}

fingerprintFiles <- function(paths) {
  if (length(paths) == 0) {
    return(NULL)
  }
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) {
    return(NULL)
  }

  # side effect: fileHashCache*.rds is written
  return(vapply(paths, FUN.VALUE = character(1), FUN = function(path) {
    if (file.exists(paste0(path, "/DONTFINGERPRINT"))) {
      return("SKIPPED")
    }
    if (dir.exists(path)) {
      files <- data.frame(
        name = robustSort(list.files(path, recursive = TRUE, full.names = FALSE)),
        stringsAsFactors = FALSE
      )
    } else {
      files <- data.frame(name = basename(path), stringsAsFactors = FALSE)
      path <- dirname(path)
    }
    if (nrow(files) == 0) {
      return(digest::digest(NULL, algo = getConfig("hash")))
    }

    files$path <- paste0(path, "/", files$name)
    files$mtime <- file.mtime(files$path)
    files$size <- file.size(files$path)
    # create key to identify entries in the hashcache which require recalculation
    files$key <- apply(files[c("name", "mtime", "size")], 1,
                       digest::digest, algo = getConfig("hash"))

    # get hash cache file name if the given path belongs to the source folder
    # (this is not the case for a redirected source folder)
    if (startsWith(normalizePath(path), normalizePath(getConfig("sourcefolder")))) {
      hashCacheFile <- paste0(getConfig("cachefolder"), "/fileHashCache", basename(path), ".rds")
    } else {
      hashCacheFile <- NULL
    }

    # hash cache files must have these columns, otherwise they are ignored/overwritten
    hashCacheColumns <- c("name", "mtime", "size", "key", "hash")
    if (!is.null(files) && !is.null(hashCacheFile) && file.exists(hashCacheFile)) {
      tryResult <- try({
        filesCache <- readRDS(hashCacheFile)
        # keep only entries which are still up-to-date
        filesCache <- filesCache[filesCache$key %in% files$key, ]
        if (nrow(filesCache) == 0 || !setequal(colnames(filesCache), hashCacheColumns)) {
          filesCache <- NULL
        } else {
          files <- files[!(files$key %in% filesCache$key), ]
        }
        if (nrow(files) == 0) {
          files <- NULL
        }
      }, silent = TRUE)
      if (inherits(tryResult, "try-error")) {
        warning("Ignoring corrupt hashCacheFile: ", as.character(tryResult))
        filesCache <- NULL
      }
    } else {
      filesCache <- NULL
    }

    if (!is.null(files)) {
      # hash the first 300 bytes of each file, or the entire file if a
      # `.fullhash` file is present in the directory
      files$hash <- vapply(
        files$path,
        digest::digest,
        character(1),
        algo = getConfig("hash"),
        file = TRUE,
        length = ifelse(file.exists(file.path(path, ".fullhash")), Inf, 300)
      )
      files$path <- NULL
    }
    files <- rbind(filesCache, files)
    files <- files[robustOrder(files$name), ]

    if (!is.null(hashCacheFile)) {
      tryCatch({
        stopifnot(setequal(colnames(files), hashCacheColumns))
        saveRDS(files, file = hashCacheFile, compress = getConfig("cachecompression"))
        Sys.chmod(hashCacheFile, mode = "0666", use_umask = FALSE)
      }, error = function(error) {
        warning("Saving hashCacheFile failed: ", error)
      })
    }

    files$mtime <- NULL
    files$key <- NULL
    return(digest::digest(files, algo = getConfig("hash")))
  }))
}
