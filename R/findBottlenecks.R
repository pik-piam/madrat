#' findBottlenecks
#'
#' Analyzes a log from a retrieveData run or from a script calling calcOutput()/readSource()
#' directly, extracts runtime information for all called functions and identifies most
#' critical bottlenecks.
#'
#' @param file path to a log file or content of a log as character vector
#' @param cumulative boolean deciding whether calls to the same function should be aggregated or not
#' @param unit unit for runtime information, either "s" (seconds), "min" (minutes) or "h" (hours)
#' @return A named list with one entry per retrieveData call found in the log, plus a "standalone"
#' entry collecting all calls that do not belong to any retrieveData call (e.g. calcOutput/readSource
#' calls made directly from a script). The names are the retrieveData types (or "standalone") and
#' each entry is a data.frame sorted by net runtime showing for the different data processing
#' functions their total runtime "time" (including all sub-functions) and net runtime "net"
#' (excluding sub-functions) and their share of total runtime. For the "standalone" entry the total
#' runtime is the sum of its top-level call runtimes, not wall clock time, as it excludes any
#' non-madrat time between those calls.
#' @author Jan Philipp Dietrich
#' @family analysis
#' @export
findBottlenecks <- function(file, unit = "min", cumulative = TRUE) {
  x <- .parseMadratLog(file)
  # Only Exit records carry a runtime; Run and [memory] records are not needed here.
  x <- x[!is.na(x$"time[s]"), , drop = FALSE]
  if (nrow(x) == 0) {
    warning("No function calls with runtime information could be detected in the log!")
    return(stats::setNames(list(), character(0)))
  }

  segments <- .splitLogByRetrieve(x)
  out <- stats::setNames(list(), character(0))
  for (type in names(segments)) {
    out[[type]] <- .analyzeBottlenecks(segments[[type]], type, unit, cumulative)
  }
  return(out)
}

.analyzeBottlenecks <- function(x, type, unit, cumulative) {
  rownames(x) <- NULL
  x$"net[s]" <- NA # nolint
  runtime <- rep(0, max(x$level) + 3)
  for (i in seq_len(nrow(x))) {
    l <- x$level[i] + 2
    runtime[l] <- runtime[l] + x$"time[s]"[i]
    x$"net[s]"[i] <- x$"time[s]"[i] - runtime[l + 1]
    runtime[l + 1] <- 0
  }

  # Root level is -1 for retrieveData blocks (forced above), else the minimum level present.
  # Computed before the optional cumulative aggregation below, which can merge root-level rows
  # into fewer, differently leveled rows and so distort the total.
  rootLevel <- min(x$level)
  totalruntime <- sum(x$"time[s]"[x$level == rootLevel])
  th   <- floor(totalruntime / 3600)
  tmin <- floor((totalruntime - th * 3600) / 60)
  ts   <- floor(totalruntime - th * 3600 - tmin * 60)
  message("Total runtime (", type, "): ", th, " hours ", tmin, " minutes ", ts, " seconds")

  if (cumulative) {
    out <- NULL
    for (cl in unique(x$class)) {
      y <- x[x$class == cl, ]
      for (i in unique(y$type)) {
        z <- y[y$type == i, ]
        z$`time[s]`[1] <- sum(z$`time[s]`)
        z$`net[s]`[1] <- sum(z$`net[s]`)
        out <- rbind(out, z[1, ])
      }
    }
    x <- out
  }

  if (unit == "min") {
    x$"time[min]" <- round(x$"time[s]" / 60, 2) # nolint
    x$"net[min]" <- round(x$"net[s]" / 60, 2) # nolint
  } else if (unit == "h") {
    x$"time[h]" <- round(x$"time[s]" / 60 / 60, 2) # nolint
    x$"net[h]" <- round(x$"net[s]" / 60 / 60, 2) # nolint
  }

  x$"time[%]" <- round(x$"time[s]" / totalruntime * 100, 2) # nolint
  x$"net[%]" <- round(x$"net[s]" / totalruntime * 100, 2) # nolint
  x <- x[robustOrder(x$"net[s]", decreasing = TRUE), ]

  if (unit %in% c("min", "h")) {
    x$"time[s]" <- NULL # nolint
    x$"net[s]" <- NULL # nolint
  }

  x <- x[c("level", "class", "type", grep("time", names(x), value = TRUE), grep("net", names(x), value = TRUE))]
  return(x)
}
