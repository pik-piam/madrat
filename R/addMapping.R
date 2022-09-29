#' addMapping
#'
#' Function whichs adds another mapping to the current list of extramappings
#' in the madrat configuration (see \code{\link{setConfig}}) and stores
#' the mapping in the mapping folder as well as output folder.
#'
#' @param filename The name of the the region mapping that should added including
#' file ending (e.g. "regionmappingREMIND.csv"). Supported formats are currently
#' ".csv" and ".rds".
#' @param mapping Mapping provided as data.frame, or NULL. If a mapping is
#' provided the data will be written in the mapping file of the given file
#' (potentially replacing existing data). If NULL the mapping from the given
#' file is used.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}
#' @examples
#' \dontrun{
#' addMapping("regionmappingH12.csv")
#' }
#' @export
addMapping <- function(filename, mapping = NULL) {

 setWrapperInactive("wrapperChecks")

 if (!is.character(filename)) {
   stop("Provided filename is not a character!")
 }

 if (is.null(mapping)) {
   mapping <- toolGetMapping(filename, type = "regional")
 } else if (!is.data.frame(mapping)) {
   stop("Cannot handle this mapping format!")
 }

 fnames <- c(file.path(getConfig("mappingfolder"), "regional", filename),
             file.path(getConfig("outputfolder"), filename))

 filetype <- tolower(file_ext(filename))

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
 setConfig(extramappings = unique(c(getConfig("extramappings"), filename)), .local = parent.frame())

}
