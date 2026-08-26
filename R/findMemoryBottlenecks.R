#' findMemoryBottlenecks
#'
#' Analyzes a log from a retrieveData run for which \code{setConfig(memoryProfiling = TRUE)} was
#' active, and identifies which processing stages drive the memory requirement of the run.
#'
#' @param file path to a log file or content of a log as character vector
#' @param cumulative boolean deciding whether calls to the same function should be aggregated or not
#' @param unit unit for memory information, either "MB" (megabytes) or "GB" (gigabytes)
#' @return A named list with one entry per retrieveData call found in the log, plus a "standalone"
#' entry collecting all calls that do not belong to any retrieveData call (e.g. calcOutput/readSource
#' calls made directly from a script). The names are the retrieveData types (or "standalone") and
#' each entry is a data.frame sorted by peak memory usage, showing for the different data processing
#' functions their peak memory usage "peak" (the highest resident set size seen during the stage),
#' their share of the block's peak, and their memory "growth" (the change in resident set size from
#' before to after the stage, summed across calls of the same type if \code{cumulative = TRUE}). If
#' \code{cumulative = FALSE} the usage before ("start") and after ("end") each individual call is
#' reported as well.
#' @author Patrick Rein
#' @seealso \code{\link{setConfig}}, \code{\link{findBottlenecks}}
#' @family analysis
#' @export
findMemoryBottlenecks <- function(file, unit = "MB", cumulative = TRUE) {
  x <- .parseMadratLog(file)
  if (!any(x$marker == "memory")) {
    warning("No memory profiling information found in the log! Was setConfig(memoryProfiling = TRUE) active?")
  }

  # Each retrieveData call marks the end of one block. Split the log into one segment per
  # retrieveData call and analyze each independently.
  segments <- .splitLogByRetrieve(x)
  out <- stats::setNames(list(), character(0))
  for (type in names(segments)) {
    # Run/Exit records carry no memory numbers and are dropped here.
    segment <- segments[[type]][segments[[type]]$marker == "memory", , drop = FALSE]
    if (nrow(segment) == 0) {
      warning("No memory profiling information found for retrieveData call \"", type, "\", skipping it.")
      next
    }
    out[[type]] <- .analyzeMemoryBottlenecks(segment, type, unit, cumulative)
  }
  return(out)
}

.analyzeMemoryBottlenecks <- function(x, type, unit, cumulative) {
  rownames(x) <- NULL

  if (cumulative) {
    out <- NULL
    for (i in unique(x$type)) {
      rows <- x[x$type == i, ]
      z <- rows[1, ]
      z$calls <- nrow(rows)
      # peaks do not add up across repeated calls, but growth does
      z$"peak[MB]" <- max(rows$"peak[MB]")
      z$"growth[MB]" <- sum(rows$"growth[MB]")
      out <- rbind(out, z)
    }
    x <- out
    # start/end are absolute snapshots and become meaningless once aggregated across calls
    x$"start[MB]" <- NULL
    x$"end[MB]" <- NULL
  }

  message("Peak memory (", type, "): ", max(x$"peak[MB]"), " MB | total growth: ",
          sum(x$"growth[MB]"), " MB")
  x$"peak[%]" <- round(x$"peak[MB]" / max(x$"peak[MB]") * 100, 2)
  x <- x[robustOrder(x$"peak[MB]", decreasing = TRUE), ]

  if (unit == "GB") {
    mbCols <- grep("\\[MB\\]$", names(x))
    x[mbCols] <- lapply(x[mbCols], function(v) round(v / 1024, 2))
    names(x) <- sub("\\[MB\\]$", "[GB]", names(x))
  }

  cols <- c("level", "class", "type")
  if (cumulative) cols <- c(cols, "calls")
  x <- x[c(cols, grep("\\[(MB|GB|%)\\]$", names(x), value = TRUE))]
  return(x)
}
