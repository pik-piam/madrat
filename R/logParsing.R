# Internals shared by findBottlenecks and findMemoryBottlenecks for reading, classifying and
# segmenting a madrat diagnostics log. .parseMadratLog() is the single entry point: it turns a
# log file/content into one row per Run/Exit/[memory] record, with retrieveData block membership
# attached, so the two analysis functions never need to know the log's line format themselves.

# Parses a madrat diagnostics log (a file path, or its content as a character vector) into one
# row per Run/Exit/[memory] record, with columns level, class, type, marker, "time[s]", "peak[MB]",
# "start[MB]", "end[MB]", "growth[MB]", block, blockType.
.parseMadratLog <- function(file) {
  f <- .readMadratLog(file)
  marker <- .logRecordType(f)
  f <- f[!is.na(marker)]
  marker <- marker[!is.na(marker)]

  x <- .parseLogCalls(f)
  x$marker <- marker
  x <- cbind(x, .logRuntimeField(f), .logMemoryFields(f))
  x <- cbind(x, .retrieveDataBlocks(x))
  return(x)
}

# Splits a parsed log into segments named by retrieveData type, plus "standalone" for rows outside any block.
.splitLogByRetrieve <- function(aLog) {
  segments <- stats::setNames(list(), character(0))
  for (id in sort(unique(aLog$block[!is.na(aLog$block)]))) {
    rows <- aLog[which(aLog$block == id), , drop = FALSE]
    segments[[rows$blockType[1]]] <- rows
  }
  standalone <- aLog[is.na(aLog$block), , drop = FALSE]
  if (nrow(standalone) > 0) {
    segments[["standalone"]] <- standalone
  }
  return(segments)
}

.readMadratLog <- function(file) {
  if (length(file) > 1 || any(grepl("\n", file))) {
    f <- unlist(strsplit(file, "\n"))
  } else {
    f <- readLines(file)
  }
  return(.mergeSplitLogLines(f))
}

# Rejoin log entries split across lines when they exceed maxLengthLogMessage. A Run/Exit
# record is complete once .logRecordType recognizes it.
.mergeSplitLogLines <- function(logLines) {
  acc <- NULL
  accPrefix <- NULL
  allLines <- character(0)
  for (line in logLines) {
    prefix <- regmatches(line, regexpr("^~*", line))
    if (!is.null(acc) && accPrefix == prefix) {
      rest <- trimws(sub("^~*\\s*", "", line))
      acc <- paste(trimws(acc), rest)
      if (!is.na(.logRecordType(acc))) {
        # We have hit the end of a Run/Exit record, stop accumulation
        allLines <- c(allLines, acc)
        acc <- NULL
        accPrefix <- NULL
      }
    } else {
      if (!is.null(acc)) {
        allLines <- c(allLines, acc)
      }
      notARunExitMemoryLine <- is.na(.logRecordType(line))
      if (grepl("^~*\\s*(Run|Exit)\\b", line) && notARunExitMemoryLine) {
        acc <- line
        accPrefix <- prefix
      } else {
        allLines <- c(allLines, line)
        acc <- NULL
        accPrefix <- NULL
      }
    }
  }
  if (!is.null(acc)) {
    allLines <- c(allLines, acc)
  }
  return(allLines)
}

# Classifies each (already merged) log line as "run", "exit", "memory" or NA (any other line,
# e.g. NOTE/cache/statistics lines, which carry no call information and are dropped).
.logRecordType <- function(logLines) {
  type <- rep(NA_character_, length(logLines))
  type[grepl("^~*\\s*Run\\s+[[:alpha:]._][[:alnum:]._]*\\(", logLines)] <- "run"
  type[grepl("^~*\\s*Exit\\b", logLines) & grepl("in [0-9.]* seconds", logLines)] <- "exit"
  type[grepl("[memory]", logLines, fixed = TRUE)] <- "memory"
  return(type)
}

# Derives nesting level, wrapper class and data type from lines documenting a call, e.g.
# "Run calcOutput(...)" or "[memory] calcOutput(...): ...". Nesting is read from the "~"-prefix.
.parseLogCalls <- function(callLogLine) {
  if (length(callLogLine) == 0) {
    return(data.frame(level = integer(0), class = character(0), type = character(0)))
  }
  x <- data.frame(level = nchar(gsub("^(~*).*$", "\\1", callLogLine)))
  x$class <- NA
  x$class[grepl("readSource", callLogLine)] <- "read"
  x$class[grepl("downloadSource", callLogLine)] <- "download"
  x$class[grepl("calcOutput", callLogLine)] <- "calc"
  x$class[grepl("retrieveData", callLogLine)] <- "retrieve"
  if (anyNA(x$class)) {
    warning("Some classes could not be properly detected!")
    x$class[is.na(x$class)] <- "unknown"
  }
  x$type <- gsub("([\"= ]|type)", "", gsub("^[^(]*\\(([^,)]*)[),].*$", "\\1", callLogLine))
  # retrieveData is never nested inside another madrat call, but vcat's level = "-" step (see
  # toolendmessage) prints "Exit retrieveData" at the same "~"-depth as its own children; force
  # it to level -1 so it is treated as their parent, not their sibling.
  x$level[x$class == "retrieve"] <- -1
  return(x)
}

# Extracts the "in <seconds> seconds" runtime from an Exit line.
.logRuntimeField <- function(exitLogLine) {
  matches <- regmatches(exitLogLine, regexec("in ([0-9.]*) seconds", exitLogLine))
  values <- vapply(matches, function(m) {
    if (length(m) == 0) {
      return(NA_real_)
    }
    return(as.numeric(m[2]))
  }, numeric(1))
  return(data.frame("time[s]" = values, check.names = FALSE))
}

# Extracts the four "<field> <number> MB" values written by reportMemoryProfiling; NA on other lines.
.logMemoryFields <- function(memoryLogLine) {
  pattern <- paste0("peak (-?[0-9]+) MB \\| start (-?[0-9]+) MB \\| ",
                    "end (-?[0-9]+) MB \\| growth (-?[0-9]+) MB")
  matches <- regmatches(memoryLogLine, regexec(pattern, memoryLogLine))
  values <- t(vapply(matches, function(m) {
    if (length(m) == 0) {
      return(rep(NA_real_, 4))
    }
    return(as.numeric(m[-1]))
  }, numeric(4)))
  colnames(values) <- c("peak[MB]", "start[MB]", "end[MB]", "growth[MB]")
  return(as.data.frame(values))
}

# Assigns each row of a parsed log to the retrieveData block it belongs to: an integer id in
# "block" (in order of appearance, NA outside any block) and the block's retrieveData type in
# "blockType" (NA outside). If an Exit has no matching Run (e.g. a truncated or pre-marker log),
# the block is assumed to start right after the previous one (or at row 1).
.retrieveDataBlocks <- function(madratLog) {
  isOpen  <- madratLog$marker == "run" & madratLog$class == "retrieve"
  isClose <- madratLog$marker == "exit" & madratLog$class == "retrieve"

  block <- rep(NA_integer_, nrow(madratLog))
  blockType <- rep(NA_character_, nrow(madratLog))
  blockId <- 0L
  openLine <- NA_integer_
  lastClose <- 0L
  for (i in seq_len(nrow(madratLog))) {
    if (isOpen[i]) {
      openLine <- i
    } else if (isClose[i]) {
      blockId <- blockId + 1L
      start <- if (!is.na(openLine)) openLine else lastClose + 1L
      block[start:i] <- blockId
      blockType[start:i] <- madratLog$type[i]
      openLine <- NA_integer_
      lastClose <- i
    }
  }
  return(data.frame(block = block, blockType = blockType, stringsAsFactors = FALSE))
}
