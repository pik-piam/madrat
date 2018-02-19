#' Tool: regionscode
#' 
#' Given a regionmapping (mapping between ISO countries and regions) the
#' function calculates a regionscode which is basically the md5sum of a reduced
#' form of the mapping. The regionscode is unique for each regionmapping and
#' can be used to clearly identify a given regionmapping. In addition several
#' checks are performed to make sure that the given input is a proper
#' regionmapping
#' 
#' 
#' @param mapping Either a path to a mapping or an already read-in mapping as
#' data.frame.
#' @return A md5-based regionscode which describes the given mapping
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{fingerprint}}, \code{\link[digest]{digest}}
#' @examples
#' 
#' file <- system.file("extdata","regionmappingH12.csv",package="madrat")
#' regionscode(file)
#' 
#' @importFrom digest digest
#' @export
regionscode <- function(mapping) {
  if(is.character(mapping)) mapping <- read.csv(mapping,sep=";")
  
  #remove first column if data has 3 columns
  if(ncol(mapping)==3) mapping[[1]] <- NULL
  
  if(ncol(mapping)!=2) stop("Regionmapping has wrong number of columns. Data must have 2 columns (1 with ISO countries and 1 with the target regional setup) + an optional first columns with names of the countries!")
  
  iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "madrat"),row.names=NULL)
  iso_country1<-as.vector(iso_country[,"x"])
  names(iso_country1)<-iso_country[,"X"]
  isocountries <- sort(iso_country1)
  
  if(nrow(mapping)>length(isocountries)) stop("Provided regionmapping has more rows than there are ISO countries in the ISO reference list. Please check the mapping!")
  if(nrow(mapping)<length(isocountries)) stop("Provided regionmapping has less rows than there are ISO countries in the ISO reference list. Please check the mapping!")
  
  lists_agree <- NULL
  for(i in 1:ncol(mapping)) {
    lists_agree <- c(lists_agree,all(isocountries==sort(as.vector(mapping[[i]]))))
  }
  
  if(!any(lists_agree)) stop("Provided regionmapping does not contain a iso country column which agrees with the reference list of ISO countries! Please check the mapping!")
  
  # Reorder if only second column contains ISO countries
  if(!lists_agree[1]) mapping <- mapping[2:1]
  
  tmp <- sort(paste(as.vector(mapping[[1]]),as.vector(mapping[[2]]),sep="."))
  return(digest(tmp,"md5"))
}
