memoryLines <- function(messages) {
  return(grep("[memory]", messages, fixed = TRUE, value = TRUE))
}

memoryValues <- function(line) {
  values <- as.numeric(regmatches(line, gregexpr("-?[0-9]+(?= MB)", line, perl = TRUE))[[1]])
  names(values) <- c("peak", "start", "end", "growth")
  return(values)
}

memoryPayload <- function(description) {
  return(list(x = as.magpie(0), isocountries = FALSE, weight = NULL, unit = "1", description = description))
}

# ~80MB. Has to be well above anything the R heap may already hold in reserve, otherwise the
# allocation is served from existing pages and never reaches the kernel's high-water mark.
ballastMb <- 80
allocateBallast <- function() {
  ballast <- runif(1e7)
  rm(ballast)
  return(invisible(NULL))
}

localProfiling <- function(env = parent.frame()) {
  testthat::skip_if_not(memoryProfilingSupported(), "requires the Linux /proc interface")
  setConfig(verbosity = 1, memoryProfiling = TRUE, .verbose = FALSE, .local = env)
}

test_that("getProcMemory returns plausible values", {
  skip_if_not(memoryProfilingSupported(), "requires the Linux /proc interface")
  memory <- getProcMemory()
  expect_named(memory, c("rss", "peak"))
  expect_true(all(memory > 0))
  expect_gte(memory[["peak"]], memory[["rss"]])
})

test_that("startMemoryProfiling resets the kernel peak", {
  skip_if_not(memoryProfilingSupported(), "requires the Linux /proc interface")
  allocateBallast()
  invisible(gc())
  start <- startMemoryProfiling()
  expect_true(is.numeric(start) && length(start) == 1)
  # after the reset the peak must have dropped back to the current usage
  memory <- getProcMemory()
  expect_equal(memory[["peak"]], memory[["rss"]])
})

test_that("reportMemoryProfiling does nothing without a profile", {
  expect_silent(reportMemoryProfiling(NULL, "calcOutput(type = \"Whatever\")"))
})

test_that("setConfig warns if memory profiling is unsupported", {
  local_mocked_bindings(memoryProfilingSupported = function() return(FALSE))
  expect_warning(localConfig(memoryProfiling = TRUE, .verbose = FALSE), "Linux /proc interface")
})

test_that("nothing is reported if memory profiling is unsupported", {
  local_mocked_bindings(memoryProfilingSupported = function() return(FALSE))
  localConfig(verbosity = 1, memoryProfiling = TRUE, .verbose = FALSE, .cfgchecks = FALSE)
  calcMemUnsupported <- function() return(memoryPayload("MemUnsupported"))
  globalassign("calcMemUnsupported")

  messages <- capture_messages(calcOutput("MemUnsupported", aggregate = FALSE))
  expect_length(memoryLines(messages), 0)
})

test_that("memory profiling is inactive by default", {
  localConfig(verbosity = 1, .verbose = FALSE)
  calcMemDefault <- function() return(memoryPayload("MemDefault"))
  globalassign("calcMemDefault")

  messages <- capture_messages(calcOutput("MemDefault", aggregate = FALSE))
  expect_length(memoryLines(messages), 0)
})

test_that("memory profiling reports one line per top-level calcOutput call", {
  localProfiling()
  calcMemHungry <- function() {
    allocateBallast() # transient, so it shows up in the peak but not at the end
    return(memoryPayload("MemHungry"))
  }
  globalassign("calcMemHungry")

  messages <- capture_messages(calcOutput("MemHungry", aggregate = FALSE))
  lines <- memoryLines(messages)
  expect_length(lines, 1)
  expect_match(lines, "calcOutput(type = \"MemHungry\"", fixed = TRUE)

  values <- memoryValues(lines)
  expect_true(all(values[c("peak", "start", "end")] > 0))
  expect_gte(values[["peak"]], values[["end"]])
  expect_equal(values[["growth"]], values[["end"]] - values[["start"]], tolerance = 1)
  # the transient allocation must show up in the peak even though it is gone at the end
  expect_gt(values[["peak"]] - values[["start"]], ballastMb / 2)

  # the memory line belongs to the exit message and therefore comes after it
  expect_lt(max(grep("Exit calcOutput", messages)), min(grep("[memory]", messages, fixed = TRUE)))
})

test_that("nested calcOutput calls are not profiled", {
  localProfiling()
  calcMemInner <- function() return(memoryPayload("MemInner"))
  calcMemOuter <- function() {
    return(list(x = calcOutput("MemInner", aggregate = FALSE),
                isocountries = FALSE, weight = NULL, unit = "1", description = "MemOuter"))
  }
  globalassign("calcMemInner", "calcMemOuter")

  messages <- capture_messages(calcOutput("MemOuter", aggregate = FALSE))
  lines <- memoryLines(messages)
  expect_length(lines, 1)
  expect_match(lines, "calcOutput(type = \"MemOuter\"", fixed = TRUE)
})

test_that("a long call string does not split the memory line across log lines", {
  localProfiling()
  setConfig(maxLengthLogMessage = 5000, .verbose = FALSE)
  calcMemLongCall <- function(...) return(memoryPayload("MemLongCall"))
  globalassign("calcMemLongCall")

  messages <- capture_messages(calcOutput("MemLongCall", aggregate = FALSE, hugeArg = strrep("x", 250)))
  lines <- memoryLines(messages)
  expect_length(lines, 1)
  # a newline followed by more content would indicate the entry got wrapped mid-message
  expect_false(grepl("\n.", lines))
})
