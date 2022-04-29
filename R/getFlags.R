#' getFlags
#'
#' Support function which extracts flags from code. Flags are string literals
#' in a function body, for example `"!# @pucArguments extra"`.
#'
#' @param code A character vector with code from functions to be analyzed
#' @return A list of found flag entries
#' @importFrom stringi stri_split stri_extract_all
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCode}}

getFlags <- function(code) {
  if (inherits(code, "function")) {
    code <- c(code = paste(deparse(code), collapse = "\n"))
  }
  flags <- stri_extract_all(code, regex = '"\\!#.*?[^\\\\]\\"', omit_no_match = TRUE)
  names(flags) <- names(code)
  flags <- flags[vapply(flags, length, integer(1)) > 0]
  if (length(flags) == 0) return(NULL)

  x <- unlist(flags, use.names = FALSE)
  tmp   <- stri_split(gsub('\\"(!#)? *(@[a-zA-Z]* *)?', "", x), regex = " +")
  type <- substring(stri_extract(x, regex = "@[^ ]*"), 2)
  names(tmp) <- rep(names(flags), vapply(flags, length, integer(1)))
  out <- list()
  for (t in unique(type)) {
    out[[t]] <- tmp[type == t]
    if (anyDuplicated(names(out[[t]]))) {
      tmp2 <- list()
      for (n in unique(names(out[[t]]))) {
        tmp2[[n]] <- unique(unlist(out[[t]][names(out[[t]]) == n], use.names = FALSE))
      }
      out[[t]] <- tmp2
    } else {
      out[[t]] <- lapply(out[[t]], unique)
    }
  }
  return(out)
}
