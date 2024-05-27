#' getMadratGraph
#'
#' Function returns the madrat graph of all linkages of full, calc, and read functions of the given madrat
#' based packages. Linkages to subfunctions of read functions (i.e. download, correct or convert functions)
#' are not listed separately, but collectively referred to through the corresponding read function.
#'
#'
#' @param packages A character vector with packages for which the available Sources/Calculations should be returned
#' @param globalenv Boolean deciding whether sources/calculations in the global environment should be included or not
#' @return A data frame with 4 columns: from (source function), from_package (package the source function originates
#' from), to (function which is using the source), to_package (package of the using function)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCalculations}}, \code{\link{getConfig}}
#' @importFrom stringi stri_match_all_regex stri_extract_all
#' @export


getMadratGraph <- function(packages = installedMadratUniverse(), globalenv = getConfig("globalenv")) {

  if (is.null(getOption("MadratCache"))) options(MadratCache = new.env(size = NA)) # nolint

  .graphHash <- function(packages, globalenv) {
    mtimes <- as.character(file.mtime(.libPaths())) # nolint
    if (globalenv) {
      f <- grep("^(read|download|convert|correct|calc|full|tool)", ls(envir = .GlobalEnv),
                perl = TRUE, value = TRUE)
      if (length(f) > 0) globalenv <- sapply(mget(f, envir = .GlobalEnv), deparse) # nolint
      else globalenv <- FALSE
    }
    return(paste0("GH", digest(c(mtimes, robustSort(packages), globalenv), algo = getConfig("hash"))))
  }

  gHash <- .graphHash(packages, globalenv)
  if (exists(gHash, envir = getOption("MadratCache"))) return(get(gHash, getOption("MadratCache")))

  # read in source code
  code <- getCode(packages = packages, globalenv = globalenv)

  # extract read/calc calls
  pattern <- "(readSource|calcOutput)\\( *([^=\"',]*=|) *(\"|')?([^\"',]*)[\"']?"
  matches <- stri_match_all_regex(code, pattern, omit_no_match = TRUE)
  names(matches) <- names(code)
  tmpfun <- function(x, l) {
    if (length(l[[x]]) == 0) return(NULL)
    out <- cbind(x, paste0(substring(l[[x]][, 2], 1, 4), l[[x]][, 5]), l[[x]])
    return(out[, c(1:4, 6:7)])
  }
  out <- lapply(names(matches), tmpfun, matches)
  out <- do.call(rbind, out)
  # clean up output
  colnames(out) <- c("to", "from", "raw", "class", "quote", "type")
  out <- as.data.frame(out, stringsAsFactors = FALSE)

  # extract tool calls
  pattern <- "\\btool([^( :]*)\\("
  matches <- stri_match_all_regex(code, pattern, omit_no_match = TRUE)
  names(matches) <- names(code)
  tmpfun <- function(x, l) {
    if (length(l[[x]]) == 0) return(NULL)
    out <- cbind(x, l[[x]])
    return(out)
  }
  out2 <- lapply(names(matches), tmpfun, matches)
  out2 <- do.call(rbind, out2)
  # clean up output
  colnames(out2) <- c("to", "from", "type")
  out2 <- as.data.frame(out2, stringsAsFactors = FALSE)
  out2$from <- sub("(", "", out2$from, fixed = TRUE)
  out2$class <- "tool"


  # set from info to NA for cases in which call statement could not be read properly
  out$from[is.na(out$quote)] <- NA
  out   <- unique(rbind(out[, c("from", "to")], out2[, c("from", "to")]))
  fpool <- attr(code, "fpool")
  out$from_package <- as.character(fpool$package[match(out$from, fpool$fname)]) # nolint
  out$to_package <- sub(":::.*$", "", out$to) # nolint
  out$to_package[!grepl(":::", out$to)] <- ".GlobalEnv"
  out$to <- sub("^.*:::", "", out$to)

  fromNA <- is.na(out$from)
  if (any(fromNA)) {
    out$from[is.na(out$from)] <- "UNKNOWN"
    out$from_package[fromNA]  <- "UNKNOWN"
    warning("Following functions contain read or calc statements which could not be identified: \n   ",
            paste(out$to[fromNA], collapse = ", "), "\n  Please adress the type explicitly in the call to allow",
            " for proper detection, e.g. readSource(\"MySource\")")
  }

  from_packageNA <- is.na(out$from_package) & !fromNA # nolint
  if (any(from_packageNA)) {
    out$from_package[from_packageNA] <- "UNKNOWN"
    warning("Following functions could not be found in the scope of packages to be checked.: \n   ",
            paste0(out$from[from_packageNA], "->", out$to[from_packageNA], collapse = ", "),
            "\n  Please make sure that they exist and adjust the scope of packages accordingly!")
  }
  # check for bidirectional package connections
  .checkBidirectional(out, details = FALSE)

  for (a in c("fpool", "hash", "mappings", "flags")) attr(out, a) <- attr(code, a)
  assign(gHash, out, envir = getOption("MadratCache"))
  return(out)
}
