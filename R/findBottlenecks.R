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
  if (length(file) > 1 || any(grepl("\n", file))) {
    f <- unlist(strsplit(file, "\n"))
  } else {
    f <- readLines(file)
  }

  f <- .mergeSplitLogLines(f)

  # Determine which lines belong to which retrieveData call.
  block <- .retrieveDataBlocks(f)

  # Only use the ends of blocks, nesting information is included in the prefix of each line.
  keep <- grepl("in [0-9.]* seconds", f)
  block <- block[keep]
  f <- f[keep]

  if (length(f) == 0) {
    warning("No function calls with runtime information could be detected in the log!")
    return(stats::setNames(list(), character(0)))
  }

  x <- data.frame(level = nchar(gsub("^(~*).*$", "\\1", f)))
  x$class <- NA
  x$class[grepl("readSource", f)] <- "read"
  x$class[grepl("downloadSource", f)] <- "download"
  x$class[grepl("calcOutput", f)] <- "calc"
  x$class[grepl("retrieveData", f)] <- "retrieve"
  if (anyNA(x$class)) {
    warning("Some classes could not be properly detected!")
    x$class[is.na(x$class)] <- "unknown"
  }
  x$level[x$class == "retrieve"] <- -1
  x$type <- gsub("([\"= ]|type)", "", gsub("^[^(]*\\(([^,)]*)[),].*$", "\\1", f))
  x$"time[s]" <- as.numeric(gsub("^.* in ([0-9.]*) seconds.*$", "\\1", f)) # nolint

  out <- list()
  for (id in sort(unique(block[!is.na(block)]))) {
    rows <- which(block == id)
    type <- x$type[rows][x$class[rows] == "retrieve"]
    out[[type]] <- .analyzeBottlenecks(x[rows, , drop = FALSE], type, unit, cumulative)
  }

  standaloneRows <- which(is.na(block))
  if (length(standaloneRows) > 0) {
    out[["standalone"]] <- .analyzeBottlenecks(x[standaloneRows, , drop = FALSE], "standalone", unit, cumulative)
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

  x <- x[c(1:3, grep("time", names(x)), grep("net", names(x)))]
  return(x)
}

.retrieveDataBlocks <- function(f) {
  # Assigns each line to the retrieveData block it belongs to (an integer id, in order of
  # appearance), or NA outside any block. retrieveData is never called from within another
  # madrat call, so blocks cannot be nested. If an Exit has no matching Run (e.g. a truncated
  # or pre-marker log), the block is assumed to start right after the previous one (or at line 1).
  isOpen  <- grepl("^~*\\s*Run\\s+retrieveData\\(", f)
  isClose <- grepl("^~*\\s*Exit\\s+retrieveData\\(.*in [0-9.]* seconds", f)

  block <- rep(NA_integer_, length(f))
  blockId <- 0L
  openLine <- NA_integer_
  lastClose <- 0L
  for (i in seq_along(f)) {
    if (isOpen[i]) {
      openLine <- i
    } else if (isClose[i]) {
      blockId <- blockId + 1L
      start <- if (!is.na(openLine)) openLine else lastClose + 1L
      block[start:i] <- blockId
      openLine <- NA_integer_
      lastClose <- i
    }
  }
  return(block)
}

.mergeSplitLogLines <- function(f) {
  # Rejoin log entries split across lines when they exceed maxLengthLogMessage. An "Exit"
  # record is complete once it contains "in ... seconds"; a "Run" record is complete once the
  # called function's name and opening parenthesis appear. This is only ever called on lines
  # (or accumulated fragments) starting with "Run" or "Exit", so exactly one of the two below
  # branches always applies.
  .isCompleteRecord <- function(line) {
    if (grepl("^~*\\s*Exit\\b", line)) return(grepl("in [0-9.]* seconds", line))
    return(grepl("^~*\\s*Run\\s+[[:alpha:]._][[:alnum:]._]*\\(", line))
  }

  acc <- NULL
  accPrefix <- NULL
  allLines <- character(0)
  for (line in f) {
    prefix <- regmatches(line, regexpr("^~*", line))
    if (!is.null(acc) && accPrefix == prefix) {
      rest <- trimws(sub("^~*\\s*", "", line))
      acc <- paste(trimws(acc), rest)
      if (.isCompleteRecord(acc)) {
        # We have hit the end of a Run/Exit record, stop accumulation
        allLines <- c(allLines, acc)
        acc <- NULL
        accPrefix <- NULL
      }
    } else {
      if (!is.null(acc)) allLines <- c(allLines, acc)
      if (grepl("^~*\\s*(Run|Exit)\\b", line) && !.isCompleteRecord(line)) {
        acc <- line
        accPrefix <- prefix
      } else {
        allLines <- c(allLines, line)
        acc <- NULL
        accPrefix <- NULL
      }
    }
  }
  if (!is.null(acc)) allLines <- c(allLines, acc)
  return(allLines)
}
