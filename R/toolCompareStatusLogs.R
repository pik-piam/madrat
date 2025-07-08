#' toolCompareStatusLogs
#'
#' Compares status logs and returns a textual rendering of the diff between the two.
#'
#' @param oldArchivePath, newArchivePath Paths to the tgz-archive that serves as the old / new archive.
#' If given, oldLogPath / newLogPath determines the name of the log file within the archive.
#' If not given, oldLogPath / newLogPath need to be given.
#' @param oldLogPath, newLogPath Paths to a log file, either within an archive,
#' if an archive is given, or to a single file.
#' @param sections List of sections the output should include.
#' Valid section names are: changedStatistics, changedCalls, addedCalls, removedCalls
#' @returns A printable string describing the changes that occurred between
#' the old and the new log.
#' @author Patrick Rein
#' @export
toolCompareStatusLogs <- function(oldArchivePath = NULL, newArchivePath = NULL,
                                  oldLogPath = file.path(".", "status.log"), newLogPath = file.path(".", "status.log"),
                                  sections = c("changedStatistics", "changedCalls", "addedCalls", "removedCalls")) {
  # Why is this a custom diff implementation instead of textual diff?
  # -> Ordering in status.log can change in all kinds of ways and is not important.
  #    Similar vein: statistics log entries have identity and should only be compared
  #    based on their content.

  if (oldArchivePath == newArchivePath && oldLogPath == newLogPath) {
    # This is also a warning that none of the relevant parameters have been changed.
    return("Nothing to do. Old and new paths are the same.")
  }

  tempDir <- withr::local_tempdir()
  if (!is.null(oldArchivePath)) {
    utils::untar(oldArchivePath,
                 files = oldLogPath,
                 exdir = file.path(tempDir, "old"))
    oldStatusLog <- yaml::read_yaml(file.path(tempDir, "old", oldLogPath))
  } else {
    oldStatusLog <- yaml::read_yaml(oldLogPath)
  }
  if (!is.null(newArchivePath)) {
    utils::untar(newArchivePath,
                 files = newLogPath,
                 exdir = file.path(tempDir, "new"))
    newStatusLog <- yaml::read_yaml(file.path(tempDir, "new", newLogPath))
  } else {
    newStatusLog <- yaml::read_yaml(newLogPath)
  }

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
                        sections = c("changedStatistics", "changedCalls", "addedCalls", "removedCalls")) {
  output <- list()

  # changed entries
  if ("changedStatistics" %in% sections) {
    changedEntriesCalls <-
      diffList[["changedCalls"]][vapply(diffList[["changedCalls"]],
                                        function(l) length(l$changedEntries) > 0, logical(1))]
    changedEntries <- lapply(changedEntriesCalls, function(aCall) {
      lapply(aCall$changedEntries, function(oldNewEntry) {
        oldData <- oldNewEntry[["old"]][["data"]]
        newData <- oldNewEntry[["new"]][["data"]]
        if (is.null(names(oldData))) {
          changedNames <- 1
        } else {
          changedNames <- Filter(function(name) {
            !identical(oldData[[name]], newData[[name]])
          },
          names(oldData))
        }

        changes <- lapply(changedNames, function(n) {
          paste0(oldData[changedNames], " -> ", newData[changedNames])
        })
        names(changes) <- changedNames
        c(list("statistic" = oldNewEntry[["old"]][["statistic"]]), changes)
      })
    })
    if (length(changedEntries) > 0) {
      changedEntries <- c(list(comment = "These statistics have changed from the old to the new log."),
                          changedEntries)
      output <- c(output, list("Changed Statistics" = changedEntries))
    }
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
    if (length(addedEntries) > 0 || length(removedEntries) > 0) {
      addRemoveEntries <- list("comment" = "These calls have changes beyond changed statistics.",
                              "Added Entries" = addedEntries,
                              "Removed Entries" = removedEntries)
      output <- c(output, list("Changed Calls" = addRemoveEntries))
    }
  }

  # added/removed calls
  if ("addedCalls" %in% sections && length(diffList[["addedCalls"]]) > 0) {
    addedCalls <- c(list("# These calls were added"),
                    diffList[["addedCalls"]])
    output <- c(output, list("Added Calls" = addedCalls))
  }

  if ("removedCalls" %in% sections && length(diffList[["removedCalls"]]) > 0) {
    removedCalls <- c(list("# These calls were removed"),
                      diffList[["removedCalls"]])
    output <- c(output, list("Removed Calls" = removedCalls))
  }

  return(yaml::as.yaml(output))
}