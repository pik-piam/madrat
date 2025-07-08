#' toolCompareStatusLogStatistics
#'
#' TODO
#' @param oldLogPath 
#' @param newLogPath 
#' @author Patrick Rein
#' @export
toolStatisticsCompareStatusLogs <- function(oldLogPath, newLogPath) {
  # Why is this a custom diff implementation instead of textual diff?
  # -> Ordering in status.log can change in all kinds of ways and is not important.
  #    Similar vein: statistics log entries have identity and should only be compared
  #    based on their content.
  oldStatusLog <- yaml::read_yaml(oldLogPath)
  newStatusLog <- yaml::read_yaml(newStatusLog)
  diff <- .compareStatusLogsStatistics(oldStatusLog, newStatusLog)
  .outputDiff(diff)
}

.compareStatusLogsStatistics <- function(oldLog, newLog) {
  # Assumed structure: list(string = call(list(entry)), entry = string|list

  # Detect added/removed calls
  removedCalls <- as.list(setdiff(names(oldLog), names(newLog)))
  addedCalls <- as.list(setdiff(names(newLog), names(oldLog)))

  # Detect changed calls
  sameCalls <- as.list(intersect(names(newLog), names(oldLog)))
  names(sameCalls) <- sameCalls
  changedCalls <- lapply(sameCalls, function(callName) {
    .changesBetweenCalls(oldLog[[callName]], newLog[[callName]])
  })
  changedCalls <- Filter(Negate(is.null), changedCalls)

  return(list(addedCalls = addedCalls, removedCalls = removedCalls, changedCalls = changedCalls))
}

.changesBetweenCalls <- function(oldEntries, newEntries) {

  # entries can be strings or lists -> unify based on unique names
  uniqueName <- function(entry) {
    if (is.character(entry)) {
      entry
    } else if (is.list(entry) && all(c("statistic", "type") %in% names(entry))) {
      paste0(entry[["type"]], entry[["statistic"]])
    } else {
      digest::digest(entry, algo = "sha256")
    }
  }
  names(oldEntries) <- lapply(oldEntries, uniqueName)
  names(newEntries) <- lapply(newEntries, uniqueName)

  # check whether identity is really unique
  assertNamesAreUnique <- function(l) {
    if (length(unique(names(l))) != length(names(l))) {
      warning("There were non-unique entries in the log: ", l)
    }
  }
  assertNamesAreUnique(oldEntries)
  assertNamesAreUnique(newEntries)

  # added/removed entries
  removedEntries <- unname(oldEntries[setdiff(names(oldEntries), names(newEntries))])
  addedEntries <- unname(newEntries[setdiff(names(newEntries), names(oldEntries))])

  # same entries
  sameEntriesNames <- as.list(intersect(names(newEntries), names(oldEntries)))

  # detect changed entries
  entryChanges <- lapply(sameEntriesNames, function(entryName) {
    if (!.entriesSame(oldEntries[[entryName]], newEntries[[entryName]])) {
      list(oldEntries[[entryName]], newEntries[[entryName]])
    } else {
      NULL
    }
  })
  entryChanges <- Filter(Negate(is.null), entryChanges)

  if (length(c(addedEntries, removedEntries, entryChanges)) == 0) {
    return(NULL)
  } else {
    list(addedEntries = addedEntries,
         removedEntries = removedEntries,
         changedEntries = entryChanges)
  }
}

.entriesSame <- function(oldEntry, newEntry) {
  isNamedList <- function(x) is.list(x) && length(names(x)) > 0
  if (isNamedList(oldEntry) && isNamedList(newEntry) && setequal(names(oldEntry), names(newEntry))) {
    return(all(vapply(names(oldEntry), function(entryName) {
      .entriesSame(oldEntry[[entryName]], newEntry[[entryName]])
    }, logical(1))))
  } else if (is.list(oldEntry) && is.list(newEntry)) {
    # Unnamed lists
    return(setequal(oldEntry, newEntry))
  }

  return(typeof(oldEntry) == typeof(newEntry) && oldEntry == newEntry)
}

.outputDiff <- function(diffList) {
  return("TODO")
}