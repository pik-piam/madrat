#' Tool: fingerprint
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
#' @param details Boolean indicating whether additional details in form
#' of an attribute with underlying hash information should be added or not
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}.
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @return A fingerprint (hash) of all provided sources, or "fingerprintError"
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{readSource}}
#' @examples
#' madrat:::fingerprint("toolGetMapping", package = "madrat")
#' @importFrom digest digest

fingerprint <- function(name, details = FALSE, graph = NULL, ...) {
  dependencies <- getDependencies(name, direction = "in", self = TRUE, graph = graph, ...)
  result <- tryCatch({
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
    out <- digest(list("v2", unname(fingerprint)), algo = getConfig("hash"))

    if (details) {
      attr(out, "details") <- fingerprint
      vcat(3, "hash components (", out, "):", show_prefix = FALSE)
      for (n in names(fingerprint)) {
        vcat(3, "  ", fingerprint[n], " | ", basename(n), " | ", n, show_prefix = FALSE)
      }
    }
    out
  },
  error = function(error) {
    vcat(2, paste(" - Fingerprinting failed:", error), show_prefix = FALSE)
    return("fingerprintError")
  })
  attr(result, "call") <- dependencies$call[dependencies$func == name]
  return(result)
}

fingerprintCall <- function(name) {
  .tmp <- function(x) {
    f <- try(eval(parse(text = x)), silent = TRUE)
    if ("try-error" %in% class(f)) {
      return(NULL)
    }
    return(digest(paste(deparse(f), collapse = " "), algo = getConfig("hash")))
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
  .tmp <- function(path) {
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
      files <- NULL
    } else {
      files$path <- paste0(path, "/", files$name)
      files$mtime <- file.mtime(files$path)
      files$size <- file.size(files$path)
      # create key to identify entries which require recalculation
      files$key <- apply(files[c("name", "mtime", "size")], 1,
        digest,
        algo = getConfig("hash")
      )
    }

    getHashCacheName <- function(path) {
      # return file name for fileHash cache if the given path belongs to the source folder
      # (this is not the case for a redirected source folder), otherwise return NULL
      if (dir.exists(getConfig("sourcefolder")) &&
            startsWith(normalizePath(path), normalizePath(getConfig("sourcefolder")))) {
        return(paste0(getConfig("cachefolder"), "/fileHashCache", basename(path), ".rds"))
      } else {
        return(NULL)
      }
    }
    hashCacheFile <- getHashCacheName(path)
    orgFiles <- files

    if (!is.null(files) && !is.null(hashCacheFile) && file.exists(hashCacheFile)) {
      tryResult <- try({
        filesCache <- readRDS(hashCacheFile)
        # keep only entries which are still up-to-date
        filesCache <- filesCache[filesCache$key %in% files$key, ]
        files <- files[!(files$key %in% filesCache$key), ]
        if (nrow(filesCache) == 0) filesCache <- NULL
        if (nrow(files) == 0) files <- NULL
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
        digest,
        character(1),
        algo = getConfig("hash"),
        file = TRUE,
        length = ifelse(file.exists(file.path(path, ".fullhash")), Inf, 300)
      )
      files$path <- NULL
      if (!is.null(hashCacheFile)) {
        if (!dir.exists(dirname(hashCacheFile))) {
          dir.create(dirname(hashCacheFile), recursive = TRUE)
        }
        tryCatch({
          saveRDS(orgFiles, file = hashCacheFile, compress = getConfig("cachecompression"))
          Sys.chmod(hashCacheFile, mode = "0666", use_umask = FALSE)
        }, error = function(error) {
          warning("Saving hashCacheFile failed: ", error)
        })
      }
    }
    files <- rbind(filesCache, files)
    files$mtime <- NULL
    files$key <- NULL
    return(digest(files[robustOrder(files$name), ], algo = getConfig("hash")))
  }
  return(sapply(paths, .tmp)) # nolint
}
