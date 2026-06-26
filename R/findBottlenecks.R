#' findBottlenecks
#'
#' Analyzes a log from a retrieveData run, extracts runtime information for all called functions
#' and identifies most critical bottlenecks.
#'
#' @param file path to a log file or content of a log as character vector
#' @param cumulative boolean deciding whether calls to the same function should be aggregated or not
#' @param unit unit for runtime information, either "s" (seconds), "min" (minutes) or "h" (hours)
#' @return A named list with one entry per retrieveData call found in the log. The names are the
#' retrieveData types and each entry is a data.frame sorted by net runtime showing for the different
#' data processing functions their total runtime "time" (including the execution of all sub-functions)
#' and net runtime "net" (excluding the runtime of sub-functions) and their share of total runtime.
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
  # Only use the ends of blocks, nesting information is included in the prefix of each line.
  f <- grep("in [0-9.]* seconds", f, value = TRUE)

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

  # Each retrieveData line marks the end of one retrieveData block. Split the log into one
  # segment per retrieveData call and analyze each independently.
  retrieveIdx <- which(x$class == "retrieve")
  if (length(retrieveIdx) == 0) {
    warning("No retrieveData call could be detected in the log!")
    return(stats::setNames(list(), character(0)))
  }
  if (max(retrieveIdx) < nrow(x)) {
    warning("Log lines after the last retrieveData call were ignored (incomplete block).")
  }

  starts <- c(1, utils::head(retrieveIdx, -1) + 1)
  out <- list()
  for (k in seq_along(retrieveIdx)) {
    segment <- x[starts[k]:retrieveIdx[k], , drop = FALSE]
    type <- x$type[retrieveIdx[k]]
    out[[type]] <- .analyzeBottlenecks(segment, type, unit, cumulative)
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

  totalruntime <- sum(x$"time[s]"[x$level == -1])
  th   <- floor(totalruntime / 3600)
  tmin <- floor((totalruntime - th * 3600) / 60)
  ts   <- floor(totalruntime - th * 3600 - tmin * 60)
  message("Total runtime (", type, "): ", th, " hours ", tmin, " minutes ", ts, " seconds")
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

.mergeSplitLogLines <- function(f) {
  # Rejoin split log entries (long messages exceed maxLengthLogMessage and wrap to next line)
  # This is done by looking for Exit lines that have no "in ... seconds".
  # In that case, the loop starts accumulating content until it hits a line with "in ... seconds"
  acc <- NULL
  accPrefix <- NULL
  allLines <- character(0)
  for (line in f) {
    prefix <- regmatches(line, regexpr("^~*", line))
    if (!is.null(acc) && accPrefix == prefix) {
      rest <- trimws(sub("^~*\\s*", "", line))
      acc <- paste(trimws(acc), rest)
      if (grepl("in [0-9.]* seconds", acc)) {
        # We have hit the end of an exit line, stop accumulation
        allLines <- c(allLines, acc)
        acc <- NULL
        accPrefix <- NULL
      }
    } else {
      if (!is.null(acc)) allLines <- c(allLines, acc)
      if (grepl("Exit", line) && !grepl("in [0-9.]* seconds", line)) {
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
