#' downloadSource
#' 
#' Download a source. The function is a wrapper for specific functions designed
#' for the different possible source types.
#' 
#' 
#' @param type source type, e.g. "IEA". A list of all available source types
#' can be retrieved with function \code{\link{getSources}("download")}.
#' @param subtype For some sources there are subtypes of the source, for these
#' source the subtype can be specified with this argument. If a source does not
#' have subtypes, subtypes should not be set.
#' @param overwrite Boolean deciding whether existing data should be
#' overwritten or not.
#' @importFrom yaml write_yaml
#' @author Jan Philipp Dietrich, David Klein
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- downloadSource("Tau", subtype="historical")
#' }
#' 
#' @export 
downloadSource <- function(type,subtype=NULL,overwrite=FALSE) {
  startinfo <- toolstartmessage("+")
  on.exit(toolendmessage(startinfo,"-"))
  
  # check type input
  if(!all(is.character(type)) || length(type)!=1) stop("Invalid type (must be a single character string)!")
  if(!is.null(subtype) && (!all(is.character(subtype)) || length(subtype)!=1)) stop("Invalid subtype (must be a single character string)!")
  
  functionname <- prepFunctionName(type=type, prefix="download")

  if(!grepl("subtype=subtype",functionname,fixed=TRUE)) subtype <- NULL
  
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  if(!file.exists(getConfig("sourcefolder"))) dir.create(getConfig("sourcefolder"), recursive = TRUE)
  
  typesubtype <- paste(c(type,subtype),collapse="/")
  
  setwd(getConfig("sourcefolder"))
  if(file.exists(typesubtype)) {
    if(overwrite) {
      unlink(typesubtype,recursive = TRUE)
    } else {
      stop("Source folder for source \"",typesubtype,"\" does already exist! Delete folder or activate overwrite to proceed!")
    }
  }
  dir.create(typesubtype, recursive = TRUE)
  setwd(typesubtype)
  on.exit(if(length(dir())==0) unlink(getwd(), recursive = TRUE), add=TRUE, after = FALSE)
  meta <- eval(parse(text=functionname))
  
  # define mandatory elements of meta data and check if they exist
  mandatory <- c("url","authors","title","license")
  if(!all(mandatory %in% names(meta))) {vcat(0, paste0("Missing entries in the meta data of function '",functionname[1],"': ",mandatory[!mandatory %in% names(meta)]))}
  
  # define reserved elements of meta data and check if they already exist
  reserved <- c("type","subtype","origin","date")
  if(any(reserved %in% names(meta))) {vcat(0, paste0("The following entries in the meta data of the function '",functionname[1],"' are reserved and will be overwritten: ",reserved[reserved %in% names(meta)]))}
  
  # set reserved meta data elements
  meta$type    <- type
  meta$subtype <- ifelse(is.null(subtype), "none",subtype)
  meta$origin  <- paste0(gsub("\\s{2,}"," ",paste(deparse(match.call()),collapse=""))," -> ",functionname," (madrat ",packageDescription("madrat")$Version," | ",attr(functionname,"pkgcomment"),")")
  meta$date    <- date()
  #meta$quality <- quality()
  
  write_yaml(meta,"DOWNLOAD.yml")
}
