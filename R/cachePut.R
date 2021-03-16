#' Tool: cachePut
#' 
#' Save data to cache
#' 
#' @param x data that should be written to cache
#' @param prefix function prefix (e.g. "calc" or "read")
#' @param type output type (e.g. "TauTotal")
#' @param args a list of named arguments used to call the given function
#' @param graph A madrat graph as returned by \code{\link{getMadratGraph}}. 
#' Will be created with \code{\link{getMadratGraph}} if not provided.
#' @param ... Additional arguments for \code{\link{getMadratGraph}} in case
#' that no graph is provided (otherwise ignored)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{cachePut}}, \code{\link{cacheName}}
#' @examples
#' \dontrun{
#'   example <- 1
#'   madrat:::cachePut(example, "calc","Example", packages="madrat")
#'  }
#' @importFrom digest digest

cachePut <- function(x, prefix, type, args=NULL, graph = NULL, ...) {
  fname <- cacheName(prefix = prefix, type = type, args = args,  graph = graph, mode = "put", ...) 
  if (!dir.exists(dirname(fname))) dir.create(dirname(fname), recursive = TRUE)
  attr(x,"cachefile") <- basename(fname)
  vcat(2," - saving data to", fname, fill = 300, show_prefix = FALSE)
  saveRDS(x, file = fname, compress = getConfig("cachecompression"))
  Sys.chmod(fname, mode = "0666", use_umask = FALSE)
}
