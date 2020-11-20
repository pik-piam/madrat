#' Tool: SubtypeSelect
#' 
#' This function is a support function for the selection of a subtype in a
#' readX function. In addition to the subtype selection it also performs some
#' consistency checks.
#' 
#' 
#' @param subtype A chosen subtype (character)
#' @param files A named vector or list. The names of the vector correspond to the
#' allowed subtypes and the content of the vector are the corresponding file
#' names.
#' @return The file name corresponding to the given subtype
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' files <-  c(protection="protection.csv",
#'               production="production.csv",
#'               extent="forest_extent.csv")
#' toolSubtypeSelect("extent",files)
#' 
#' @export
toolSubtypeSelect <- function(subtype, files) {
    if(is.null(subtype)) stop('Subtype has to be set! Available subtypes are: ',paste(names(files),collapse=", "))
    if(!(subtype %in% names(files))) stop('Unknown subtype "',subtype,'"! Available subtypes are: ',paste(names(files),collapse=", "))
    return(files[[subtype]])
} 
