# Internals shared by findBottlenecks and findMemoryBottlenecks for reading, classifying and
# segmenting a madrat diagnostics log. .parseMadratLog() is the single entry point: it turns a
# log file/content into one row per Run/Exit/[memory] record, with retrieveData block membership
# attached, so the two analysis functions never need to know the log's line format themselves.

# Patterns identifying a complete (i.e. not split across lines) Run/Exit record. Shared between
# .mergeSplitLogLines (deciding when an accumulated fragment is done) and .logRecordType
# (classifying already-merged lines), so the record format is stated in one place.
.reRunOpen <- "^~*\\s*Run\\s+[[:alpha:]._][[:alnum:]._]*\\("
.reExit <- "^~*\\s*Exit\\b"
.reRuntime <- "in [0-9.]* seconds"

.readMadratLog <- function(file) {
  if (length(file) > 1 || any(grepl("\n", file))) {
    f <- unlist(strsplit(file, "\n"))
  } else {
    f <- readLines(file)
  }
  return(.mergeSplitLogLines(f))
}

# Rejoin log entries split across lines when they exceed maxLengthLogMessage. An "Exit"
# record is complete once it contains "in ... seconds"; a "Run" record is complete once the
# called function's name and opening parenthesis appear. This is only ever called on lines
# (or accumulated fragments) starting with "Run" or "Exit", so exactly one of the two below
# branches always applies.
.mergeSplitLogLines <- function(f) {
  .isCompleteRecord <- function(line) {
    if (grepl(.reExit, line)) return(grepl(.reRuntime, line))
    return(grepl(.reRunOpen, line))
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

# Classifies each (already merged) log line as "run", "exit", "memory" or NA (any other line,
# e.g. NOTE/cache/statistics lines, which carry no call information and are dropped).
.logRecordType <- function(f) {
  type <- rep(NA_character_, length(f))
  type[grepl(.reRunOpen, f)] <- "run"
  type[grepl(.reExit, f) & grepl(.reRuntime, f)] <- "exit"
  type[grepl("[memory]", f, fixed = TRUE)] <- "memory"
  return(type)
}

# Derives nesting level, wrapper class and data type from lines documenting a call, e.g.
# "Run calcOutput(...)" or "[memory] calcOutput(...): ...". Nesting is read from the "~"-prefix.
.parseLogCalls <- function(f) {
  if (length(f) == 0) {
    return(data.frame(level = integer(0), class = character(0), type = character(0)))
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
  x$type <- gsub("([\"= ]|type)", "", gsub("^[^(]*\\(([^,)]*)[),].*$", "\\1", f))
  # retrieveData is never nested inside another madrat call, but vcat's level = "-" step (see
  # toolendmessage) prints "Exit retrieveData" at the same "~"-depth as its own children; force
  # it to level -1 so it is treated as their parent, not their sibling.
  x$level[x$class == "retrieve"] <- -1
  return(x)
}

# Extracts the "in <seconds> seconds" runtime from an Exit line, in a single pass over f.
# NA on lines that carry no runtime.
.logRuntimeField <- function(f) {
  matches <- regmatches(f, regexec("in ([0-9.]*) seconds", f))
  values <- vapply(matches, function(m) {
    if (length(m) == 0) return(NA_real_)
    return(as.numeric(m[2]))
  }, numeric(1))
  return(data.frame("time[s]" = values, check.names = FALSE))
}

# Extracts the four "<field> <number> MB" values written together by reportMemoryProfiling, in a
# single pass over f. All four are NA on lines that carry no memory report.
.logMemoryFields <- function(f) {
  pattern <- paste0("peak (-?[0-9]+) MB \\| start (-?[0-9]+) MB \\| ",
                    "end (-?[0-9]+) MB \\| growth (-?[0-9]+) MB")
  matches <- regmatches(f, regexec(pattern, f))
  values <- t(vapply(matches, function(m) {
    if (length(m) == 0) return(rep(NA_real_, 4))
    return(as.numeric(m[-1]))
  }, numeric(4)))
  colnames(values) <- c("peak[MB]", "start[MB]", "end[MB]", "growth[MB]")
  return(as.data.frame(values))
}

# Assigns each row of a parsed log to the retrieveData block it belongs to: an integer id in
# "block" (in order of appearance, NA outside any block) and the block's retrieveData type in
# "blockType" (NA outside). retrieveData is never called from within another madrat call, so
# blocks cannot be nested. If an Exit has no matching Run (e.g. a truncated or pre-marker log),
# the block is assumed to start right after the previous one (or at row 1).
.retrieveDataBlocks <- function(x) {
  isOpen  <- x$marker == "run" & x$class == "retrieve"
  isClose <- x$marker == "exit" & x$class == "retrieve"

  block <- rep(NA_integer_, nrow(x))
  blockType <- rep(NA_character_, nrow(x))
  blockId <- 0L
  openLine <- NA_integer_
  lastClose <- 0L
  for (i in seq_len(nrow(x))) {
    if (isOpen[i]) {
      openLine <- i
    } else if (isClose[i]) {
      blockId <- blockId + 1L
      start <- if (!is.na(openLine)) openLine else lastClose + 1L
      block[start:i] <- blockId
      blockType[start:i] <- x$type[i]
      openLine <- NA_integer_
      lastClose <- i
    }
  }
  return(data.frame(block = block, blockType = blockType, stringsAsFactors = FALSE))
}

# Parses a madrat diagnostics log (a file path, or its content as a character vector) into one
# row per Run/Exit/[memory] record, with columns level/class/type/marker/"time[s]"/"peak[MB]"/
# "start[MB]"/"end[MB]"/"growth[MB]"/block/blockType. This is the single entry point
# findBottlenecks and findMemoryBottlenecks use to go from raw log text to a tidy data.frame.
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

# Splits a parsed log into one segment per retrieveData call, plus a "standalone" segment
# collecting all rows that do not belong to any retrieveData call (e.g. calcOutput/readSource
# calls made directly from a script). Returns a named list of data.frame segments, named by
# retrieveData type (or "standalone").
.splitLogByRetrieve <- function(x) {
  segments <- stats::setNames(list(), character(0))
  for (id in sort(unique(x$block[!is.na(x$block)]))) {
    rows <- x[which(x$block == id), , drop = FALSE]
    segments[[rows$blockType[1]]] <- rows
  }
  standalone <- x[is.na(x$block), , drop = FALSE]
  if (nrow(standalone) > 0) {
    segments[["standalone"]] <- standalone
  }
  return(segments)
}
