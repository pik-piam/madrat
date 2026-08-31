# float32 ULP at magnitude v: the gap between adjacent float32 values in v's
# binade. write.magpie stores 23 mantissa bits (IEEE-754 single), so an .mz
# round-trip cannot resolve a difference smaller than this -- it is pure
# storage rounding, not a computation difference.
.float32Ulp <- function(v) {
  v <- abs(v)
  out <- numeric(length(v))
  nz <- v > 0
  out[nz] <- 2^(floor(log2(v[nz])) - 23)
  out
}

# Classify every differing element of two equal-length numeric arrays into
# noise / zero-flip / genuine / NA-mismatch buckets. Runs in chunks so memory
# stays bounded by chunkSize rather than by the full arrays; x and y must
# already be loaded (this does not read anything from disk).
#
# `float32` picks the noise rule: TRUE applies a float32-ULP tolerance, which
# is only justified when at least one side went through .mz's float32
# storage; FALSE falls back to a relative `reassocTol`, appropriate for
# formats (.rds, .cs3/.cs4, ...) that store doubles throughout and so have no
# rounding step to blame for small gaps.
#
# zero-flip: one side is exactly 0 and the other is dust (< zeroEps) -- e.g. a
# reordered floating-point computation flipping a near-zero value across a
# downstream `x < 0 -> 0` clamp. Gated by zeroEps so a real 0-vs-large
# difference is never miscounted as noise. Not automatically safe to ignore:
# a dust-sized flip can be amplified by a downstream consumer that branches on
# exact zero.
.classifyDiffs <- function(x, y, float32, zeroEps = 1e-6, reassocTol = 1e-9, chunkSize = 5e6) {
  n <- length(x)
  nNoise <- 0L
  nZeroFlip <- 0L
  nGenuine <- 0L
  nNAmismatch <- 0L
  sumAbsDiff <- 0
  sumAbsY <- 0
  maxGenuineRel <- 0
  maxGenuineAbs <- 0
  genuineExampleIdx <- NA_integer_

  pos <- 0L
  while (pos < n) {
    chunkLen <- min(chunkSize, n - pos)
    idx <- (pos + 1):(pos + chunkLen)
    oldChunk <- x[idx]
    newChunk <- y[idx]
    sumAbsY <- sumAbsY + sum(abs(newChunk), na.rm = TRUE)

    isNaOld <- is.na(oldChunk)
    isNaNew <- is.na(newChunk)
    nNAmismatch <- nNAmismatch + sum(xor(isNaOld, isNaNew))
    bothPresent <- !isNaOld & !isNaNew
    diffIdx <- which(bothPresent & oldChunk != newChunk)

    if (length(diffIdx)) {
      oldVal <- oldChunk[diffIdx]
      newVal <- newChunk[diffIdx]
      delta <- newVal - oldVal
      absDelta <- abs(delta)
      sumAbsDiff <- sumAbsDiff + sum(absDelta)
      # diffIdx already excludes oldChunk == newChunk, so at least one of
      # oldVal, newVal is nonzero and this denominator is always > 0.
      relDiff <- absDelta / pmax(abs(oldVal), abs(newVal))

      # when exactly one side is 0, abs(delta) equals the magnitude of the other side
      isZeroFlip <- ((oldVal == 0) != (newVal == 0)) & absDelta < zeroEps
      bothNonzero <- oldVal != 0 & newVal != 0

      if (float32) {
        u <- .float32Ulp(pmax(abs(oldVal), abs(newVal)))
        isNoise <- bothNonzero & !isZeroFlip & absDelta <= 2 * u
      } else {
        isNoise <- bothNonzero & !isZeroFlip & relDiff <= reassocTol
      }
      isGenuine <- !isZeroFlip & !isNoise

      nZeroFlip <- nZeroFlip + sum(isZeroFlip)
      nNoise    <- nNoise    + sum(isNoise)
      nGenuine  <- nGenuine  + sum(isGenuine)

      if (any(isGenuine)) {
        genuineIdx <- which(isGenuine)
        worstGenuineIdx <- genuineIdx[which.max(relDiff[genuineIdx])]
        if (relDiff[worstGenuineIdx] > maxGenuineRel) {
          maxGenuineRel <- relDiff[worstGenuineIdx]
          maxGenuineAbs <- absDelta[worstGenuineIdx]
          genuineExampleIdx <- pos + diffIdx[worstGenuineIdx]
        }
      }
    }
    pos <- pos + chunkLen
  }

  list(n = n, nNoise = nNoise, nZeroFlip = nZeroFlip, nGenuine = nGenuine,
       nNAmismatch = nNAmismatch,
       nDiff = nNoise + nZeroFlip + nGenuine + nNAmismatch,
       sumAbsDiff = sumAbsDiff, sumAbsY = sumAbsY,
       maxGenuineRel = maxGenuineRel, maxGenuineAbs = maxGenuineAbs,
       genuineExampleIdx = genuineExampleIdx)
}

#' compareData
#'
#' Compares the content of two data archives and looks for similarities and differences
#'
#' @param x Either a tgz file or a folder containing data sets
#' @param y Either a tgz file or a folder containing data sets
#' @param tolerance tolerance level below which differences will
#' get ignored
#' @param yearLim year until when the comparison should be performed.
#' Useful to check if data is identical until a certain year.
#' @param detailed if TRUE, files that differ are additionally broken down into
#' storage/reassociation noise, zero-flips (one side exactly 0, the other dust --
#' see the "zero-flip" note in the printed report), and genuine differences. This
#' is purely diagnostic: it doesn't change the OK/DIFF verdict.
#' @return Invisibly, a list with the ok/skip/diff/miss counts, the file lists, and
#' (if \code{detailed = TRUE}) a \code{details} list of per-file difference
#' statistics keyed by file name.
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @family validation
#' @seealso \code{\link{setConfig}}, \code{\link{calcTauTotal}},
#' @importFrom utils untar
#' @importFrom withr local_tempdir
#' @export

compareData <- function(x, y, tolerance = 10^-5, # nolint: cyclocomp_linter
                        yearLim = NULL, detailed = FALSE) {
  tDir <- local_tempdir()

  .getDir <- function(tDir, file, name) {
    if (dir.exists(file)) return(file)
    d <- file.path(tDir, name)
    if (file.exists(d)) unlink(d, recursive = TRUE, force = TRUE)
    untar(file, exdir = d)
    return(d)
  }
  xDir <- .getDir(tDir, x, "x")
  yDir <- .getDir(tDir, y, "y")

  out <- list(ok = 0, skip = 0, diff = 0, miss = 0)
  out$files <- list(notInX = setdiff(list.files(yDir), list.files(xDir)),
                    notInY = setdiff(list.files(xDir), list.files(yDir)),
                    inBoth = intersect(list.files(xDir), list.files(yDir)))

  maxchar <- max(vapply(out$files$inBoth, nchar, integer(1)))
  out$miss <- length(out$files$notInA) + length(out$files$notInA)

  .reportMissingFiles <- function(x, name) {
    if (length(x) > 0) {
      message(length(x), " file(s) missing in ", name, ": ", paste(x, collapse = ", "))
    }
  }
  .reportMissingFiles(out$files$notInX, "x")
  .reportMissingFiles(out$files$notInY, "y")

  .dimEqual <- function(x, y) {
    equal <- TRUE
    for (i in 1:3) {
      if (!setequal(dimnames(x)[[i]], dimnames(y)[[i]])) equal <- FALSE
    }
    return(equal)
  }

  .rmag <- function(f, yearLim) {
    x <- try(read.magpie(f), silent = TRUE)
    if (!is.magpie(x)) {
      return(NULL)
    } else {
      if (!is.null(yearLim)) x <- x[, getYears(x, as.integer = TRUE) <= yearLim, ]
    }
    attr(x, "comment") <- NULL
    return(x)
  }

  .hashFile <- function(f) {
    if (!file.exists(f)) {
      return(NULL)
    }

    if (1 == system(paste("file -b", f, "| grep -q text"))) {
      x <- system(paste("md5sum", f), intern = TRUE)
    } else {
      x <- system(paste("grep -v '^\\*'", f, "| md5sum"), intern = TRUE)
    }

    if (is.null(attr(x, "status"))) x else NULL
  }

  # Report the noise/zero-flip/genuine/NA-mismatch breakdown for one differing
  # file pair. Returns the stats list, or NULL if the two objects could not be
  # aligned into comparable flat arrays (e.g. duplicate dimnames within a
  # dimension).
  .reportDetailed <- function(x, y, xFile, yFile) {
    if (!identical(dimnames(x), dimnames(y))) {
      aligned <- try(y[dimnames(x)[[1]], dimnames(x)[[2]], dimnames(x)[[3]]], silent = TRUE)
      if (inherits(aligned, "try-error")) {
        message("             detailed comparison skipped (dimnames not alignable)")
        return(NULL)
      }
      y <- aligned
    }

    float32 <- any(grepl("\\.mz$", c(xFile, yFile), ignore.case = TRUE))
    st <- .classifyDiffs(x@.Data, y@.Data, float32 = float32)
    noiseLabel <- if (float32) "float32-ULP" else "reassoc-noise"

    message(sprintf("             elements %d | differing %d (%.4f%%)",
                    st$n, st$nDiff, 100 * st$nDiff / st$n))
    message(sprintf("             %s %d | zero-flip %d | genuine %d | NA-mismatch %d",
                    noiseLabel, st$nNoise, st$nZeroFlip, st$nGenuine, st$nNAmismatch))
    if (st$nZeroFlip > 0) {
      message("             note: zero-flips are dust-sized (< 1e-06) -- check downstream ",
              "consumers that branch on exact zero before treating them as safe")
    }
    if (st$nGenuine > 0) {
      message(sprintf("             max genuine: rel %g, abs %g at flat index %d",
                      st$maxGenuineRel, st$maxGenuineAbs, st$genuineExampleIdx))
    }
    message(sprintf("             aggregate relative error (sum|diff|/sum|y|): %g",
                    if (st$sumAbsY > 0) st$sumAbsDiff / st$sumAbsY else NA))
    return(st)
  }

  i <- 1
  for (f in out$files$inBoth) {
    counter <- format(paste0("(", i, "/", length(out$files$inBoth), ") "), width = 10)
    message(counter, format(f, width = maxchar), " ... ", appendLF = FALSE)
    i <- i + 1

    xFile <- file.path(xDir, f)
    yFile <- file.path(yDir, f)

    if ("Linux" == Sys.info()[["sysname"]]
        && identical(.hashFile(xFile), .hashFile(yFile))) {
      # checking hashes of all but the file header is much faster then checking
      # all the data
      message("OK")
      out$ok <- out$ok + 1
    } else {
      x <- .rmag(xFile, yearLim)
      y <- .rmag(yFile, yearLim)
      if (is.null(x) && is.null(y)) {
        message("skipped")
        out$skip <- out$skip + 1
      } else {
        if (!identical(dim(x), dim(y))) {
          message("!= dim")
          out$diff <- out$diff + 1
        } else if (!.dimEqual(x, y)) {
          message("!= dimnames")
          out$diff <- out$diff + 1
        } else {
          diff <- max(abs(x - y), na.rm = TRUE)
          identicalXY <- identical(x, y)
          if (!identicalXY && diff > tolerance) {
            message("!= values (max diff = ", round(diff, 8), ")")
            out$diff <- out$diff + 1
          } else {
            message("OK")
            out$ok <- out$ok + 1
          }
          if (detailed && !identicalXY) {
            st <- .reportDetailed(x, y, xFile, yFile)
            if (!is.null(st)) {
              out$details[[f]] <- st
            }
          }
        }
      }
    }
  }

  message("[OK ", out$ok, " | DIFF ", out$diff, " | SKIP ", out$skip, " | MISS ", out$miss, "]")
  return(invisible(out))
}
