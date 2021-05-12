digest <- function(...) {
  return(digest::digest(..., algo = getConfig("hash")))
}
