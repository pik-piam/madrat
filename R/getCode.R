#' getCode
#'
#' Extract function code from madrat-style functions in specified packages
#'
#' @param packages A character vector with packages for which the available Sources/Calculations should be returned
#' @param globalenv Boolean deciding whether sources/calculations in the global environment should be included or not
#' @return A named vector with condensed function code
#' @importFrom stringi stri_split stri_extract
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMadratGraph}}

getCode <- function(packages = installedMadratUniverse(), globalenv = getConfig("globalenv")) {
  # LC_CTYPE changes how \uFC is deparsed (ü vs <U+00FC>), but we want locale independent results
  ctypeLocale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "C") # nolint
  on.exit(Sys.setlocale("LC_CTYPE", ctypeLocale))  # nolint

  .extractCode <- function(x) {
    out <- deparse(eval(parse(text = x)))
    pattern <- "(^|::)read"
    if (grepl(pattern, x)) {
      extras <- c("download", "convert", "correct")
      for (e in extras) {
        tmp <- try(deparse(eval(parse(text = sub(pattern, paste0("\\1", e), x)))), silent = TRUE)
        if (!("try-error" %in% class(tmp))) out <- c(out, tmp)
      }
    }
    return(paste(out, collapse = " "))
  }

  # extract function pool
  fpool <- getCalculations("read|calc|full|tool", packages = packages, globalenv = globalenv)

  # check for duplicates
  fpool$fname <- sub("^.*:::", "", fpool$call)
  duplicated <- duplicated(fpool$fname, fromLast = TRUE)
  if (any(duplicated)) {
    for (d in fpool$fname[duplicated]) {
      warning("Duplicate functions found: ", paste(fpool$call[fpool$fname == d], collapse = ", "),
              "! Laste entry will be used")
    }
    fpool <- fpool[!duplicated, ]
  }

  # read in source code
  code <- sapply(fpool$call, .extractCode)  # nolint


  .getMappingFiles <- function(code) {
    code <- code[!(names(code) %in% paste0("madrat:::", c("toolGetMapping", "toolConvertMapping", "toolAggregate")))]
    getMappings <- stri_extract_all(code, regex = "toolGetMapping\\(([^()]*|[^(]*\\([^)]*\\)[^)]*)\\)",
                                    omit_no_match = TRUE)
    names(getMappings) <- names(code)
    getMappings <- getMappings[vapply(getMappings, length, integer(1)) > 0]
    .tmp <- function(x) {
      out <- try(eval(parse(text = x)), silent = TRUE)
      if ("try-error" %in% class(out)) return("NOTFOUND")
      return(out)
    }
    .evals <- Vectorize(.tmp, "x", USE.NAMES = FALSE)
    .getPaths <- function(x) {
      x <- gsub(" +", " ", x)
      x <- sub(", ?returnPathOnly ?= ?(FALSE|TRUE)", "", x)
      x <- sub("\\)$", ", returnPathOnly = TRUE)", x)
      return(unique(.evals(unique(x))))
    }
    getMappings <- lapply(getMappings, .getPaths)
    .warnNotfound <- function(getMappings) {
      .notfound <- function(set) return(is.element("NOTFOUND", set))
      notfound <- vapply(getMappings, .notfound, logical(1))
      notfound <- names(notfound[notfound])
      if (length(notfound) > 0) {
        for (nf in notfound) warning("Mapping in ", nf, " not found!")
      }
    }
    .warnNotfound(getMappings)
    return(getMappings)
  }

  .getFlags <- function(code) {
    flags <- stri_extract_all(code, regex = '"\\!#.*?[^\\\\]\\"', omit_no_match = TRUE)
    names(flags) <- names(code)
    flags <- flags[vapply(flags, length, integer(1)) > 0]
    if (length(flags) == 0) return(NULL)

    x <- unlist(flags, use.names = FALSE)
    tmp   <- stri_split(gsub('\\"(!#)? *(@[a-z]* *)?', "", x), regex = " +")
    type <- substring(stri_extract(x, regex = "@[^ ]*"), 2)
    names(tmp) <- rep(names(flags), vapply(flags, length, integer(1)))
    out <- list()
    for (t in unique(type)) {
      out[[t]] <- tmp[type == t]
      if (anyDuplicated(names(out[[t]]))) {
        tmp2 <- list()
        for (n in unique(names(out[[t]]))) tmp2[[n]] <- unique(unlist(out[[t]][names(out[[t]]) == n],
                                                                      use.names = FALSE))
        out[[t]] <- tmp2
      } else {
        out[[t]] <- lapply(out[[t]], unique)
      }
    }
    return(out)
  }

  attr(code, "fpool")     <- fpool
  attr(code, "hash")      <- sapply(code, digest, algo = getConfig("hash")) # nolint
  attr(code, "mappings")  <- .getMappingFiles(code)
  attr(code, "flags")     <- .getFlags(code)
  return(code)
}
