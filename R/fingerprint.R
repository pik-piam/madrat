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
#' @return A md5-based fingerprint of all provided sources
#' @author Jan Philipp Dietrich, Pascal Führlich
#' @seealso \code{\link{readSource}}
#' @examples
#' madrat:::fingerprint("toolGetMapping", package="madrat")
#' @importFrom digest digest

fingerprint <- function(name, details=FALSE, graph = NULL, ...) {
  dependencies <- getDependencies(name, direction = "in", self = TRUE, graph = graph, ...)
  result <- tryCatch({

    fingerprintFunctions <- dependencies$hash[order(dependencies$call)]
    names(fingerprintFunctions) <- dependencies$call[order(dependencies$call)]

    # handle special requests via flags
    .tmp <- function(x) return(sort(sub(":+", ":::", x), method = "radix"))
    ignore  <- .tmp(attr(dependencies, "flags")$ignore)
    monitor <- .tmp(attr(dependencies, "flags")$monitor)
    # if conflicting information is giving (monitor and ignore at the same time,
    # prioritize monitor request)
    ignore <- setdiff(ignore, monitor)
    # add calls from the monitor list which are not already monitored
    fingerprintMonitored <- fingerprintCall(setdiff(monitor, names(fingerprintFunctions)))
    # ignore functions mentioned in the ignore list
    fingerprintFunctions <- fingerprintFunctions[setdiff(names(fingerprintFunctions), ignore)]
    sources <- substring(dependencies$func[dependencies$type == "read"], 5)
    if (length(sources) > 0) {
      sources <- paste0(getConfig("sourcefolder"), "/", sort(sources, method = "radix"))
    }
    fingerprintSources <- fingerprintFiles(sources)
    fingerprintMappings <- fingerprintFiles(attr(dependencies, "mappings"))
    fingerprint <- c(fingerprintFunctions, fingerprintSources, fingerprintMappings, fingerprintMonitored)
    fingerprint <- fingerprint[order(basename(names(fingerprint)), method = "radix")]
    out <- digest(unname(fingerprint), algo = getConfig("hash"))
    if (details) {
      attr(out, "details") <- fingerprint
      vcat(3, "hash components (", out, "):", show_prefix = FALSE)
      for (n in names(fingerprint)) {
        vcat(3, "  ", fingerprint[n], " | ", basename(n), " | ", n, show_prefix = FALSE)
      }
    }
    out
  }, error = function(error) {
    vcat(2, paste(" - Fingerprinting failed:", error), show_prefix = FALSE)
    return("fingerprintError")
  })
  attr(result, "call") <- dependencies$call[dependencies$func == name]
  return(result)
}

fingerprintCall <- function(name) {
  .tmp <- function(x) {
    f <- try(eval(parse(text = x)), silent = TRUE)
    if ("try-error" %in% class(f)) return(NULL)
    return(digest(paste(deparse(f), collapse = " "), algo = getConfig("hash")))
  }
  return(unlist(sapply(name, .tmp)))
}

fingerprintFiles <- function(paths) {
  if (length(paths) == 0) return(NULL)
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) return(NULL)
  .tmp <- function(path) {
    if (dir.exists(path)) {
      filenames <- sort(list.files(path, recursive = TRUE, full.names = TRUE), method = "radix")
    } else {
      filenames <- path
    }
    # use the first 300 byte of each file and the file sizes for hashing
    fileFingerprints <- sapply(filenames, digest, algo = getConfig("hash"), file = TRUE, length = 300)
    names(fileFingerprints) <- basename(names(fileFingerprints))
    fileSizes <- file.size(filenames)
    return(digest(c(fileFingerprints, fileSizes), algo = getConfig("hash")))
  }
  return(sapply(paths, .tmp))
}
