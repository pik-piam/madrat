#' getCalculations
#'
#' This function can be used to retrieve a list of currently available
#' sources and outputs (based on the availability of corresponding conversion
#' functions in the loaded data data processing packages.)
#'
#'
#' @aliases getCalculations
#' @param prefix Type of calculations, vector of types or search term (e.g. "read|calc"). Available options are
#' "download" (source download), "read" (source read), "correct" (source corrections),
#' "convert" (source conversion to ISO countries), "calc" (further calculations),
#' and "full" (collections of calculations)
#' @param packages A character vector with packages for which the available Sources/Calculations should be returned
#' @param globalenv Boolean deciding whether sources/calculations in the global environment should be included or not
#' @return A data frame containing all currently available outputs of
#' all loaded data processing packages including its name, its function call and its package origin.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}, \code{\link{setConfig}}
#' @examples
#'
#' print(getCalculations())
#' print(getCalculations("read"))
#'
#' @export
#'
getCalculations <- function(prefix = "calc", packages = getConfig("packages"), globalenv = getConfig("globalenv")) {
  if (length(prefix) > 1) prefix <- paste(prefix, collapse = "|")
  if (globalenv) packages <- c(packages, ".GlobalEnv")

  x <- .getAllFunctions(packages)

  pattern <- paste0("^(", prefix, ")")
  x <- x[grep(pattern, x$type), ]
  if (is.null(dim(x)) || dim(x)[1] == 0) return(NULL)
  x$call <- paste0(x$package, ":::", x$type)
  x$type <- sub(pattern, "", x$type)
  x$call <- sub(".GlobalEnv:::", "", x$call, fixed = TRUE)
  x <- x[!(x$type %in% c("Source", "Output")), ]
  rownames(x) <- NULL
  return(x)
}

.getAllFunctions <- function(packages) {
  .tmp <- function(p) {
    if (p == ".GlobalEnv") {
      ns <- try(as.environment(p), silent = TRUE)
    } else {
      ns <- try(getNamespace(p), silent = TRUE)
    }
    if ("try-error" %in% class(ns)) {
      warning("Package \"", p, "\" is not available and is ignored!")
      return(NULL)
    }
    tmp <- ls(ns)
    names(tmp) <- rep(p, length(tmp))
    return(tmp)
  }
  x <- unlist(lapply(packages, .tmp))
  return(data.frame(type = x, package = names(x), stringsAsFactors = FALSE))
}
