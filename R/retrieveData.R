#' retrieveData
#' 
#' Function to retrieve a predefined collection of calculations for a specific
#' regionmapping. If the data is already processed and a modelfolder is
#' provided it will just be copied, otherwise it will be created first 
#' and eventually copied.
#' 
#' 
#' @param model The names of the model for which the data should be provided
#' (e.g. "magpie").
#' @param rev data revision which should be used/produced (positive numeric).
#' @param modelfolder main directory of the model which should receive the
#' source data. If set to NULL data will only be produced but not copied to
#' any model.
#' @param cachetype defines what cache should be used. "rev" points to a cache
#' shared by all calculations for the given revision, "def" points to the cache
#' as defined in the current settings and "tmp" temporarily creates a cache
#' folder for the calculations and deletes it afterwards again
#' @param ... (Optional) Settings that should be changed using
#' \code{\link{setConfig}} (e.g. regionmapping).
#' @author Jan Philipp Dietrich
#' @seealso
#' \code{\link{calcOutput}},\code{\link{setConfig}},\code{\link{file2destination}}
#' @examples
#' 
#' \dontrun{ 
#' retrieveData("magpie",rev=2,regionmapping="regionmappingMAgPIE.csv")
#' }
#' 
#' @export
retrieveData <- function(model, rev=0, modelfolder=NULL, cachetype="rev", ...) {
 setConfig(...)
  
 regionmapping <- getConfig("regionmapping")  
 if(!file.exists(regionmapping)) regionmapping <- toolMappingFile("regional",getConfig("regionmapping"))
 regionscode <- regionscode(regionmapping) 
 
 # save current settings to set back if needed
 forcecache_setting  <- getConfig()$forcecache
 cachefolder_setting <- getConfig()$cachefolder
 
 collectionname <- paste0(tolower(model), "_", regionscode, "_rev", rev)
 sourcefolder <- paste0(getConfig("mainfolder"), "/output/", collectionname)
 if(!file.exists(paste0(sourcefolder,".tgz"))) {
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
     cache_tmp <- paste0(getConfig("mainfolder"),"/cache/rev",rev)
   } else if(cachetype=="def") {
     cache_tmp <-  cachefolder_setting
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
   
   vcat(2," - execute function",functionname)
   x <- eval(parse(text=functionname))
   vcat(2," - function",functionname,"finished")
   
   #remove duplicates in mapping
   map <- readLines(paste0(sourcefolder,"/file2destination.csv"))
   writeLines(unique(map),paste0(sourcefolder,"/file2destination.csv"))
   
 } else {
  if(!file.exists(sourcefolder)) dir.create(sourcefolder,recursive = TRUE)
  cwd <- getwd()
  setwd(sourcefolder)
  trash <- system(paste0("tar -xvf ../",collectionname,".tgz"), intern=TRUE)
  setwd(cwd) 
  startinfo <- toolstartmessage(0)
  vcat(1," - data is already available and not calculated again.") 
 } 
 
 
 #copy data to model
 if(!is.null(modelfolder)) {
   cat("\nStart copying source data for model",model,"from", sourcefolder, "to", modelfolder)
   #read mapping
   map <- read.csv(paste0(sourcefolder,"/file2destination.csv"), sep=";", stringsAsFactors = FALSE)
   for(i in 1:nrow(map)) {
     from <- paste0(sourcefolder,"/",map[i,1])
     to <- paste0(modelfolder,"/",map[i,2],"/",map[i,1])
     file.copy(from,to,overwrite=TRUE)
     cat("  Copied",map[i,1],"to",map[i,2])
   }
 }

 
 # delete new temporary cache folder and set back configutations 
 if(exists("cache_tmp") & getConfig()$delete_cache & cachetype=="tmp") unlink(cache_tmp, recursive=TRUE)

 setConfig(cachefolder=cachefolder_setting)
 setConfig(forcecache=forcecache_setting)

 toolendmessage(startinfo)
 
 cwd <- getwd()
 setwd(sourcefolder)
 trash <- system(paste0("tar -czf ../",collectionname,".tgz"," *"), intern = TRUE)
 setwd(cwd) 
 unlink(sourcefolder, recursive = TRUE)

}
