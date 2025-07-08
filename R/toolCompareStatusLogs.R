#' toolCompareStatusLogs
#'
#' TODO
#' @param oldLogPath
#' @param newLogPath
#' @param sections
#' @author Patrick Rein
#' @export
toolCompareStatusLogs <- function(oldLogPath, newLogPath,
                                            sections = c("statistics", "changedCalls", "addedCalls", "removedCalls")) {
  # Why is this a custom diff implementation instead of textual diff?
  # -> Ordering in status.log can change in all kinds of ways and is not important.
  #    Similar vein: statistics log entries have identity and should only be compared
  #    based on their content.
  oldStatusLog <- yaml::read_yaml(oldLogPath)
  newStatusLog <- yaml::read_yaml(newLogPath)
  diff <- .compareStatusLogsStatistics(oldStatusLog, newStatusLog)
  return(.renderDiff(oldStatusLog, newStatusLog, diff, sections = sections))
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
      list(old = oldEntries[[entryName]],
           new = newEntries[[entryName]])
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

#' Renders a list of differences based on the relevance.
#' The reasoning is that changed entries should come first,
#' as they can point out subtle problems.
#' Added or removed calls or entries are less important, as they
#' require explicit edits to the pipeline to occur.
#' 1. Changed entries
#' 2. Changed calls
#'   2.1. Added entries
#'   2.2. Removed entries
#' 3. Added calls
#' 4. Removed calls
.renderDiff <- function(oldLog, newLog, diffList,
                        sections = c("statistics", "changedCalls", "addedCalls", "removedCalls")) {
  output <- list()

  # changed entries
  if ("statistics" %in% sections) {
    changedEntriesCalls <-
      diffList[["changedCalls"]][vapply(diffList[["changedCalls"]],
                                        function(l) length(l$changedEntries) > 0, logical(1))]
    changedEntries <- lapply(changedEntriesCalls, function(aCall) {
      lapply(aCall$changedEntries, function(oldNewEntry) {
        if (is.null(names(oldNewEntry[["old"]][["data"]]))) {
          changedNames <- 1
        } else {
          changedNames <- Filter(function(name) {
            !identical(oldNewEntry[["old"]][["data"]][[name]], oldNewEntry[["new"]][["data"]][[name]])
          },
          names(oldNewEntry[["old"]][["data"]]))
        }

        list("statistic" = oldNewEntry[["old"]][["statistic"]],
             "from" = oldNewEntry[["old"]][["data"]][changedNames],
             "to.." = oldNewEntry[["new"]][["data"]][changedNames])
      })
    })
    changedEntries <- c(list(comment = "These statistics have changed from the old to the new log."),
                        changedEntries)
    output <- c(output, list("Changed Statistics" = changedEntries))
  }

  # added/removed entries
  if ("changedCalls" %in% sections) {
    addedEntries <- lapply(diffList[["changedCalls"]], function(aCall) {
      aCall[["addedEntries"]]
    })
    addedEntries <- Filter(function(l) length(l) > 0, addedEntries)
    removedEntries <- lapply(diffList[["changedCalls"]], function(aCall) {
      aCall[["removedEntries"]]
    })
    removedEntries <- Filter(function(l) length(l) > 0, removedEntries)
    addRemoveEntries <- list("comment" = "These calls have changes beyond changed statistics.",
                             "Added Entries" = addedEntries,
                             "Removed Entries" = removedEntries)
    output <- c(output, list("Changed Calls" = addRemoveEntries))
  }

  # added/removed calls
  if ("addedCalls" %in% sections) {
    addedCalls <- c(list("# These calls were added"),
                    diffList[["addedCalls"]])
    output <- c(output, list("Added Calls" = addedCalls))
  }

  if ("removedCalls" %in% sections) {
    removedCalls <- c(list("# These calls were removed"),
                      diffList[["removedCalls"]])
    output <- c(output, list("Removed Calls" = removedCalls))
  }

  return(yaml::as.yaml(output))
}