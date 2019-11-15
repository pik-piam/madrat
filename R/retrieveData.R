#' retrieveData
#' 
#' Function to retrieve a predefined collection of calculations for a specific
#' regionmapping. 
#' 
#' 
#' @param model The names of the model for which the data should be provided
#' (e.g. "magpie").
#' @param rev data revision which should be used/produced. Format must be compatible to
#' \code{\link[base]{numeric_version}}.
#' @param dev development suffix to distinguish development versions for the same data
#' revision. This can be useful to distinguish parallel lines of development.
#' @param cachetype defines what cache should be used. "rev" points to a cache
#' shared by all calculations for the given revision, "def" points to the cache
#' as defined in the current settings and "tmp" temporarily creates a cache
#' folder for the calculations and deletes it afterwards again
#' @param ... (Optional) Settings that should be changed using
#' \code{\link{setConfig}} (e.g. regionmapping).
#' @author Jan Philipp Dietrich, Lavinia Baumstark
#' @seealso
#' \code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#' 
#' \dontrun{ 
#' retrieveData("example", rev="2.1.1", dev="test", regionmapping="regionmappingH12.csv")
#' }
#' 
#' @export
retrieveData <- function(model, rev=0, dev="", cachetype="rev", ...) {
 setConfig(...)
  
 regionmapping <- getConfig("regionmapping")  
 if(!file.exists(regionmapping)) regionmapping <- toolMappingFile("regional",getConfig("regionmapping"))
 regionscode <- regionscode(regionmapping) 
 
 # save current settings to set back if needed
 cfg_backup <- getOption("madrat_cfg")
 on.exit(options("madrat_cfg" = cfg_backup))

 rev <- numeric_version(rev)
 
 collectionname <- paste0("rev", rev, dev, "_", regionscode, "_", tolower(model), ifelse(getConfig("debug")==TRUE,"_debug",""))
 sourcefolder <- paste0(getConfig("mainfolder"), "/output/", collectionname)
 if(!file.exists(paste0(sourcefolder,".tgz")) || getConfig("debug")==TRUE) {
   # data not yet ready and has to be prepared first
   
   #create folder if required
   if(!file.exists(sourcefolder)) dir.create(sourcefolder,recursive = TRUE)
   
   #copy mapping to mapping folder and set config accordingly
   mappath <- toolMappingFile("regional",paste0(regionscode,".csv"),error.missing = FALSE)
   if(!file.exists(mappath)) file.copy(regionmapping,mappath)
   #copy mapping to output folder
   try(file.copy(regionmapping, sourcefolder, overwrite = TRUE))
   setConfig(regionmapping=paste0(regionscode,".csv"),
             outputfolder=sourcefolder,
             diagnostics="diagnostics")
   # make new temporary cache folder and forche the use of it
   if(cachetype=="tmp") {
     cache_tmp <- paste0(getConfig("mainfolder"),"/cache/tmp",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
   } else if(cachetype=="rev") {
     cache_tmp <- paste0(getConfig("mainfolder"),"/cache/rev",rev,dev)
   } else if(cachetype=="def") {
     cache_tmp <- getConfig("cachefolder")
   } else {
     stop("Unknown cachetype \"",cachetype,"\"!")
   }
   if(!exists(cache_tmp)) dir.create(cache_tmp, recursive = TRUE, showWarnings = FALSE)
   # change settings
   setConfig(cachefolder=cache_tmp)
   setConfig(forcecache=TRUE)
   # run full* functions
   
   startinfo <- toolstartmessage(0)
   
   functionname <- prepFunctionName(type=toupper(model), prefix="full")
   
   vcat(2," - execute function",functionname, fill=300, show_prefix=FALSE)
   x <- eval(parse(text=functionname))
   vcat(2," - function",functionname,"finished", fill=300, show_prefix=FALSE)   
   
 } else {
  if(!file.exists(sourcefolder)) dir.create(sourcefolder,recursive = TRUE)
  cwd <- getwd()
  setwd(sourcefolder)
  trash <- system(paste0("tar -xvf ../",collectionname,".tgz"), intern=TRUE)
  setwd(cwd) 
  startinfo <- toolstartmessage(0)
  vcat(-2," - data is already available and not calculated again.", fill=300) 
 } 
 
 # delete new temporary cache folder and set back configutations 
 if(exists("cache_tmp") & getConfig()$delete_cache & cachetype=="tmp") unlink(cache_tmp, recursive=TRUE)

 toolendmessage(startinfo)
 
 cwd <- getwd()
 setwd(sourcefolder)
 trash <- system(paste0("tar -czf ../",collectionname,".tgz"," *"), intern = TRUE)
 setwd(cwd) 
 unlink(sourcefolder, recursive = TRUE)

}
