#' addMapping
#'
#' Function whichs adds another mapping to the current list of extramappings
#' in the madrat configuration (see \code{\link{setConfig}}) and stores
#' the mapping in the mapping as well as output folder.
#'
#' @param name The name of the the region mapping that should added including
#' file ending (e.g. "regionmappingREMIND.csv"). Supported formats are currently
#' ".csv" and ".rds".
#' @param mapping Mapping provided as magpie object, or NULL. If a mapping is
#' provided the data will be written in the mapping file of the given file. If
#' not provided (set to NULL) and existing mapping file with the given name is
#' being looked for and used.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}
#' @examples
#' \dontrun{
#' addMapping("regionmappingH12.csv")
#' }
#' @export
addMapping <- function(name, mapping = NULL) {

 setWrapperInactive("wrapperChecks")

 if (is.null(mapping)) {
   mapping <- toolGetMapping(name, type = "regional")
 } else if (!is.data.frame(mapping)) {
   stop("Cannot handle this mapping format!")
 }

 fnames <- c(file.path(getConfig("mappingfolder"), "regional", name),
             file.path(getConfig("outputfolder"), name))

 filetype <- tolower(file_ext(name))

 for (fname in fnames) {
   d <- dirname(fname)
   if (!dir.exists(d)) dir.create(d, recursive = TRUE)
   if (filetype == "csv") {
     write.table(mapping, fname, sep = ";", quote = FALSE)
   } else if (filetype == "rds") {
     saveRDS(mapping, fname, compress = "xz")
   } else {
     stop("Unsupported filetype \"", filetype, "\"")
   }
 }
 setConfig(extramappings = unique(c(getConfig("extramappings"), name)), .local = parent.frame(2))
}
