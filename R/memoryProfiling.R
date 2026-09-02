#' memoryProfilingSupported
#'
#' Checks whether the kernel interfaces required for memory profiling are available.
#' Only Linux exposes both the high-water mark of the resident set size and a way
#' to reset it.
#'
#' @return TRUE if memory profiling can be performed, FALSE otherwise
#' @author Patrick Rein
#' @seealso \code{\link{setConfig}}
#' @keywords internal
memoryProfilingSupported <- function() {
  return(file.exists("/proc/self/status") && # nolint: absolute_path_linter.
           file.exists("/proc/self/clear_refs")) # nolint: absolute_path_linter.
}

#' getProcMemory
#'
#' Reads the current and the peak resident set size of this process as recorded
#' by the kernel.
#'
#' @return A named numeric vector with the entries "rss" (VmRSS) and "peak" (VmHWM), both in kB
#' @author Patrick Rein
#' @keywords internal
getProcMemory <- function() {
  status <- readLines("/proc/self/status", warn = FALSE) # nolint: absolute_path_linter.
  fields <- grep("^Vm(RSS|HWM):", status, value = TRUE)
  memory <- as.numeric(sub("^\\D*([0-9]+).*$", "\\1", fields))
  names(memory) <- sub("^Vm(RSS|HWM):.*$", "\\1", fields)
  return(c(rss = memory[["RSS"]], peak = memory[["HWM"]]))
}

#' startMemoryProfiling
#'
#' Starts a memory profiling block by resetting the kernel's peak resident set size
#' counter, so that the peak reported by \code{\link{reportMemoryProfiling}} refers to
#' this block only. As that counter is process wide, profiling blocks must not overlap.
#'
#' @return The resident set size at the start of the block in kB, or NULL if profiling
#' is unavailable on this system
#' @author Patrick Rein
#' @seealso \code{\link{reportMemoryProfiling}}
#' @keywords internal
startMemoryProfiling <- function() {
  if (!memoryProfilingSupported()) return(NULL)
  # writing 5 to clear_refs sets the peak (VmHWM) back to the current usage (VmRSS)
  base::cat("5\n", file = "/proc/self/clear_refs") # nolint: absolute_path_linter.
  return(getProcMemory()[["rss"]])
}

#' reportMemoryProfiling
#'
#' Ends a memory profiling block started by \code{\link{startMemoryProfiling}} and
#' writes the collected numbers to the log.
#'
#' @param start the baseline as returned by \code{\link{startMemoryProfiling}};
#' if NULL nothing is reported
#' @param callString the function call the measurement belongs to
#' @author Patrick Rein
#' @seealso \code{\link{startMemoryProfiling}}
#' @keywords internal
reportMemoryProfiling <- function(start, callString) {
  if (is.null(start)) return(invisible(NULL))
  memory <- getProcMemory()
  .mb <- function(kb) return(round(kb / 1024))
  # pasted into one string so cat's fill-wrapping (which only ever breaks between arguments,
  # never inside one) cannot split the entry across log lines; makes log analysis easier
  entry <- paste0("[memory] ", callString, ": ",
                  "peak ", .mb(memory[["peak"]]), " MB | ",
                  "start ", .mb(start), " MB | ",
                  "end ", .mb(memory[["rss"]]), " MB | ",
                  "growth ", .mb(memory[["rss"]] - start), " MB")
  vcat(1, entry, fill = 300, show_prefix = FALSE)
  return(invisible(NULL))
}
