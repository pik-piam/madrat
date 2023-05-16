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
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @seealso \code{\link{setConfig}}, \code{\link{calcTauTotal}},
#' @importFrom utils untar
#' @importFrom withr local_tempdir
#' @export

compareData <- function(x, y, tolerance = 10^-5, yearLim = NULL) {
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

  i <- 1
  for (f in out$files$inBoth) {
    counter <- format(paste0("(", i, "/", length(out$files$inBoth), ") "), width = 10)
    message(counter, format(f, width = maxchar), " ... ", appendLF = FALSE)
    i <- i + 1
    x <- .rmag(file.path(xDir, f), yearLim)
    y <- .rmag(file.path(yDir, f), yearLim)
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
        if (!identical(x, y) && diff > tolerance) {
          message("!= values (max diff = ", round(diff, 8), ")")
          out$diff <- out$diff + 1
        } else {
          message("OK")
          out$ok <- out$ok + 1
        }
      }
    }
  }
  message("[OK ", out$ok, " | DIFF ", out$diff, " | SKIP ", out$skip, " | MISS ", out$miss, "]")
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
