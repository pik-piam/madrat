#' readSource
#' 
#' Read in a source file and convert it to a MAgPIE object. The function is a
#' wrapper for specific functions designed for the different possible source
#' types.
#' 
#' 
#' @param type source type, e.g. "IEA". A list of all available source types
#' can be retrieved with function \code{\link{getSources}}.
#' @param subtype For some sources there are subtypes of the source, for these
#' source the subtype can be specified with this argument. If a source does not
#' have subtypes, subtypes should not be set.
#' @param convert Boolean indicating whether input data conversion to
#' ISO countries should be done or not. In addition it can be set to "onlycorrect" 
#' for sources with a separate correctXXX-function.
#' @return magpie object with the temporal and data dimensionality of the
#' source data. Spatial will either agree with the source data or will be on
#' ISO code country level depending on your choice for the argument "convert".
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Lavinia Baumstark
#' @seealso \code{\link{setConfig}}, ' \code{\link{downloadSource}}, 
#' \code{\link{readTau}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("Tau","paper")
#' }
#' 
#' @importFrom magclass read.magpie is.magpie updateMetadata withMetadata getComment<-
#' @importFrom methods existsFunction is
#' @export
readSource <- function(type,subtype=NULL,convert=TRUE) {
  cwd <- getwd()
  setwd(getConfig("mainfolder"))
  options(reducedHistory=TRUE)
  startinfo <- toolstartmessage("+")
  on.exit(toolendmessage(startinfo,"-"))
  
  # check type input
  if(!is.character(type) || length(type)!=1) stop("Invalid type (must be a single character string)!")
  
  # Does the cache folder exists? (only to be checked if cache is enabled) 
  if(!file.exists(getConfig("cachefolder")) & getConfig("enablecache")) dir.create(getConfig("cachefolder"),recursive = TRUE)
  
  # Does the source that should be read exist?
  if(!(type%in%getSources())) stop('Type "',type, '" is not a valid source type. Available sources are: "',paste(getSources(),collapse='", "'),'"')
  
  # Does a correctTYPE function exist?
  if(convert=="onlycorrect" & !(type %in% getSources("correct"))) {
    warning("No correct function for ",type," could be found. Set convert to FALSE.")
    convert <- FALSE
  }
  
  testISO <- function(x, allowGLO=FALSE, functionname="function") {
    if(allowGLO && length(x)==1 && x=="GLO") return()
    iso_country  <- read.csv2(system.file("extdata","iso_country.csv",package = "madrat"),row.names=NULL)
    iso_country1 <-as.vector(iso_country[,"x"])
    names(iso_country1) <-iso_country[,"X"]
    isocountries  <- sort(iso_country1)
    datacountries <- sort(x)
    if(length(isocountries)!=length(datacountries)) stop("Wrong number of countries returned by ",functionname,"!")
    if(any(isocountries!=datacountries)) stop("Countries returned by ",functionname," do not agree with iso country list!")
  }
  
  .getData <- function(type,subtype,prefix="read") {
    # get data either from cache or by calculating it from source
    sourcefolder <- paste0(getConfig("sourcefolder"),"/",type)
    if(!is.null(subtype) && file.exists(paste0(sourcefolder,"/",subtype,"/DOWNLOAD.yml"))) sourcefolder <- paste0(sourcefolder,"/",subtype)
    if(!file.exists(sourcefolder)) stop('Source folder "',sourcefolder,'" for source "',type,'" cannot be found! Please set a proper path with "setConfig"!')  

    fname <- paste0(prefix,type,subtype)
    cachefile_old <- paste0(getConfig("cachefolder"),"/",fname,".mz")
    cachefile_new <- paste0(getConfig("cachefolder"),"/",make.names(fname),".rds")
    if(!file.exists(cachefile_new) && file.exists(cachefile_old)) {
      cachefile <- cachefile_old
      mz <- TRUE
    } else {
      cachefile <- cachefile_new
      mz <- FALSE
    }
    
    .f <- function(type, prefix) {
      out <- prepFunctionName(type=type, prefix=prefix, error_on_missing=FALSE)
      if(is.null(out)) return(NULL)
      return(eval(parse(text=sub("\\(.*$","",out))))
    }
    .fp <- function(sourcefolder, type, prefix) {
      if(prefix=="read") {
        fp <- fingerprint(sourcefolder, readSource, .f(type,"read"))  
      } else if (prefix=="correct") {
        fp <- fingerprint(sourcefolder, readSource, .f(type,"read"), .f(type,"correct"))
      } else if (prefix=="convert") {
        if(!is.null(.f(type,"correct"))) {
          fp <- fingerprint(sourcefolder, readSource, .f(type,"read"), .f(type,"correct"), .f(type,"convert"))
        } else {
          fp <- fingerprint(sourcefolder, readSource, .f(type,"read"), .f(type,"convert"))
        }
      }
      return(fp)
    }
    
    fp <- .fp(sourcefolder, type, prefix)
    
    err <- try({
      if(getConfig("enablecache") && file.exists(cachefile) &&  !(fname %in% getConfig("ignorecache")) && !(type %in% getConfig("ignorecache")) ) { 
        vcat(2," - loading data", cachefile, fill=300, show_prefix=FALSE)
        if(mz) {
          x <- try(read.magpie(cachefile))
        } else {
          x <- try(readRDS(cachefile))
        }
        if(attr(x,"comment")[1] == fp | all(getConfig("forcecache")==TRUE) | fname %in% getConfig("forcecache") | type %in% getConfig("forcecache")) {
          if(attr(x,"comment")[1] == fp) {
            vcat(-2," - use cache",cachefile, fill=300)
          } else {
            vcat(-2," - force cache",cachefile, fill=300)
          }
          
          if(prefix=="convert") {
            iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "madrat"),row.names=NULL)
            iso_country1<-as.vector(iso_country[,"x"])
            names(iso_country1)<-iso_country[,"X"]
            isocountries <- sort(iso_country1)
            datacountries <- sort(getRegions(x))
            if(length(isocountries)!=length(datacountries)) stop("Wrong number of countries in ",cachefile,"!")
            if(any(isocountries!=datacountries)) stop("Countries in ",cachefile," do not agree with iso country list!")
          }
          attr(x,"id") <- fname
          return(x)
        } else {
          vcat(2," - outdated data in cache (", cachefile,"), reload source data", fill=300, show_prefix=FALSE)
        }
      }
    }, silent=TRUE)
    if(is(err,"try-error")) {
      vcat(0,as.character(attr(err,"condition")))
      vcat(-2, " - reading cache failed! Rerun without cache.")
    }
    
    if(prefix=="correct") {
      x <- .getData(type,subtype,"read")
      id <-  paste(attr(x,"id"),fname,sep="|")
    } else if(prefix=="convert") {
      if(type %in% getSources("correct")) {    
        x <- .getData(type,subtype,"correct")
      } else {
        x <- .getData(type,subtype,"read")
      }
      id <- paste(attr(x,"id"),fname,sep="|")
    } else {
      id <- fname
    }
    
    cwd <- getwd()
    setwd(sourcefolder)
    functionname <- prepFunctionName(type=type, prefix=prefix, ignore=ifelse(is.null(subtype),"subtype",NA))
    x <- eval(parse(text=functionname))
    setwd(cwd)
    if(!is.magpie(x)) stop("Output of function \"",functionname,"\" is not a MAgPIE object!")
    if(prefix=="convert") {
      testISO(getRegions(x),functionname=functionname)
    }
    vcat(2," - saving data to", cachefile_new, fill=300, show_prefix=FALSE)
    getComment(x) <- fp
    saveRDS(x, cachefile_new, compress = getConfig("cachecompression"))
    Sys.chmod(cachefile_new,"0666", use_umask=FALSE)
    attr(x,"id") <- id
    return(x)
  }
  
  # Check whether source folder exists and try do download source data if it is missing
  sourcefolder <- paste0(getConfig("sourcefolder"),"/",type)
  # if any DOWNLOAD.yml exists use these files as reference,
  # otherwise just check whether the sourcefolder exists
  df <- dir(sourcefolder, recursive=TRUE, pattern="DOWNLOAD.yml")
  if(length(df)==0) {
    source_missing <- !file.exists(sourcefolder)
  } else {
    sourcefile <- paste0(getConfig("sourcefolder"),"/",type,"/DOWNLOAD.yml")
    sourcesubfile <- paste0(getConfig("sourcefolder"),"/",type,"/",subtype,"/DOWNLOAD.yml")
    source_missing <- (!file.exists(sourcefile) && !file.exists(sourcesubfile))
  }
  
  if(source_missing) {
    # does a routine exist to download the source data?
    if(type %in% getSources("download")) {
      downloadSource(type, subtype=subtype)
    } else {
      typesubtype <- paste0(paste(c(paste0("type = \"",type),subtype),collapse="\" subtype = \""),"\"")
      stop("Sourcefolder does not contain data for the requested source ",typesubtype," and there is no download script which could provide the missing data. Please check your settings!")
    }
  }
  
  if(!is.logical(convert) && convert!="onlycorrect") stop("Unknown convert setting \"",convert,"\" (allowed: TRUE, FALSE and \"onlycorrect\") ")
        
  if(convert==TRUE & (type %in% getSources("regional"))) {
    prefix <- "convert"
  } else if (convert %in% c(TRUE, "onlycorrect") & (type %in% getSources("correct"))) {
    prefix <- "correct"
  } else {
    prefix <- "read"
  }

  x <- .getData(type,subtype,prefix)
    
  id <- attr(x,"id")
  on.exit(toolendmessage(startinfo,"-",id=id))
  
  if(convert==TRUE) {
    # make sure that data is either on ISO country level or global
    functionname <- prepFunctionName(type=type, prefix=prefix, ignore=ifelse(is.null(subtype),"subtype",NA))
    testISO(getRegions(x), allowGLO=TRUE, functionname=paste0("readSource(\"",type,"\", convert=TRUE)"))
  }
  x <- clean_magpie(x)
  x <- updateMetadata(x,calcHistory="update",cH_priority=1)
  setwd(cwd)
 
  
  return(x)
}    
    
