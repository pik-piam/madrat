#' digest
#'
#' A wrapper for digest::digest, which uses the hash algorithm given by getConfig("hash")
#'
#' @param ... arguments passed on to digest::digest
#'
#' @author Pascal Führlich
#' @seealso \code{\link[digest]{digest}}

digest <- function(...) {
  return(digest::digest(..., algo = getConfig("hash")))
}
