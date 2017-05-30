#' Tool: spatial_header
#' 
#' Given a regionmapping (mapping between ISO countries and regions) the
#' function calculates a 0.5 degree spatial header for 0.5 degree magclass
#' objects
#' 
#' @param mapping Either a path to a mapping or an already read-in mapping as
#' data.frame.
#' @return A vector with 59199 elements
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{regionscode}}
#' @examples
#' 
#' \dontrun{
#' spatial_header("regionmappingMAgPIE.csv")
#' }
#' 
#' @export
spatial_header <- function(mapping) {
  if(is.character(mapping)) {
    map <- read.csv(mapping, sep = ";")
  } else if(is.data.frame(mapping)) {
    map <- mapping
  } else {
    stop("Mapping is provided in an unsupported format. It should be either a character or a data.frame!")
  }
  regionscode <- regionscode(map)
  reg <- as.character(map$RegionCode)
  names(reg) <- as.character(map$CountryCode)
  iso_cell <- read.csv2(system.file("extdata", "iso_cell.csv", 
                                    package = "madrat"))
  cell <- iso_cell
  cell$country <- reg[as.character(cell$country)]
  spatial_header <- paste(cell$country, cell$magpiecell, 
                          sep = ".")
  return(spatial_header)
}