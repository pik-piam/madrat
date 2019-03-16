#' calcOutput
#' 
#' Calculate a specific output for which a calculation function exists package. The function is a
#' wrapper for specific functions designed for the different possible output types.
#' 
#' 
#' @param type output type, e.g. "TauTotal". A list of all available source
#' types can be retrieved with function \code{\link{getCalculations}}.
#' @param aggregate Boolean indicating whether output data aggregation should be performed or not, "GLO" (or "glo") for aggregation to one global region,
#' "REG+GLO" (or "regglo") for a combination of regional and global data.
#' @param file A file name. If given the output is written to that file in the
#' outputfolder as specified in the config.
#' @param years A vector of years that should be returned. If set to NULL all
#' available years are returned.
#' @param round A rounding factor. If set to NULL no rounding will occur.
#' @param supplementary boolean deciding whether supplementary information such as weight should be
#' returned or not. If set to TRUE a list of elements will be returned!
#' @param append boolean deciding whether the output data should be appended in the existing file.
#' Works only when a file name is given in the function call. 
#' @param na_warning boolean deciding whether NAs in the data set should create a warning or not
#' @param try if set to TRUE the calculation will only be tried and the script will continue even if
#' the underlying calculation failed. If set to TRUE calculation will stop with an error in such a 
#' case.
#' @param ... Additional settings directly forwarded to the corresponding
#' calculation function
#' @return magpie object with the requested output data either on country or on
#' regional level depending on the choice of argument "aggregate" or a list of information
#' if supplementary is set to TRUE.
#' @note The underlying calc-functions are required to provide a list of information back to 
#' \code{calcOutput}. Following list entries should be provided:
#' \itemize{
#' \item x - the data itself as magclass object
#' \item weight - a weight for the spatial aggregation
#' \item unit - unit of the provided data
#' \item description - a short description of the data
#' \item note (optional) - additional notes related to the data
#' \item isocountries (optional | default = TRUE (mostly) or FALSE (if global)) - a boolean
#' indicating whether data is in iso countries or not (the latter will deactivate several 
#' features such as aggregation)
#' \item mixed_aggregation (optional | default = FALSE) - boolean which allows for mixed 
#' aggregation (weighted mean mixed with summations). If set to TRUE weight columns 
#' filled with NA will lead to summation, otherwise they will trigger an error.
#' \item min (optional) - Minimum value which can appear in the data. If provided calcOutput
#' will check whether there are any values below the given threshold and warn in this case
#' \item max (optional) - Maximum value which can appear in the data. If provided calcOutput
#' will check whether there are any values above the given threshold and warn in this case
#' \item aggregationFunction (optional | default = toolAggregate) - Function to be used to 
#' aggregate data from country to regions. The function must have the argument \code{x} for 
#' the data itself and \code{rel} for the relation mapping between countries and regions and 
#' must return the data as magpie object in the spatial resolution as defined in rel.
#' \item aggregationArguments (optional) - List of additional, named arguments to be supplied 
#' to the aggregation function. In addition to the arguments set here, the function will be 
#' supplied with the arguments \code{x}, \code{rel} and if provided/deviating from the default
#' also \code{weight} and \code{mixed_aggregation}.
#' }
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setConfig}}, \code{\link{calcTauTotal}},
#' @examples
#' 
#' \dontrun{ 
#' 
#' a <- calcOutput(type="TauTotal")
#' 
#' }
#' 
#' @importFrom magclass nyears nregions getComment<- getComment getYears clean_magpie write.report2 write.magpie
#' getCells getYears<- is.magpie dimSums getMetadata updateMetadata
#' @importFrom utils packageDescription read.csv2
#' @export

calcOutput <- function(type,aggregate=TRUE,file=NULL,years=NULL,round=NULL,supplementary=FALSE, append=FALSE, na_warning=TRUE, try=FALSE, ...) {
 
  # check settings for aggregate
  if(!is.logical(aggregate)) {
    if(!(toupper(gsub("+","",aggregate,fixed = TRUE)) %in% c("GLO","REGGLO"))) {
      stop("Illegal setting aggregate = ",aggregate,"! Make sure that all arguments 
            which should be passed to the specific calc function are given with its name (e.g. arg=BLA)")
    }
  }
  
  # check type input
  if(!is.character(type)) stop("Invalid type (must be a character)!")
  if(length(type)!=1)     stop("Invalid type (must be a single character string)!")
  
  cwd <- getwd()
  if(is.null(getOption("gdt_nestinglevel"))) vcat(-2,"")
  startinfo <- toolstartmessage("+")
  if(!file.exists(getConfig("outputfolder"))) dir.create(getConfig("outputfolder"),recursive = TRUE)
  setwd(getConfig("outputfolder"))
  on.exit(setwd(cwd))
  functionname <- prepFunctionName(type=type, prefix="calc", ignore=ifelse(is.null(years),"years",NA))
  tmpargs <- paste(names(list(...)),list(...),sep="_",collapse="-")
  if(tmpargs!="") tmpargs <- paste0("-",make.names(tmpargs))
  fname <- paste0("calc",type,tmpargs)
  on.exit(toolendmessage(startinfo,"-",id=fname), add = TRUE)
  tmppath <- paste0(getConfig("cachefolder"),"/",fname,".Rda")
  if(((all(getConfig("forcecache")==TRUE) | fname %in% getConfig("forcecache") | type %in% getConfig("forcecache")) & !(type %in% getConfig("ignorecache"))) & !(fname %in% getConfig("ignorecache")) & file.exists(tmppath) ) {
    vcat(-2," - force cache",tmppath)
    load(tmppath) 
  } else {
    vcat(2," - execute function",functionname, show_prefix=FALSE)
    if(try) {
      x <- try(eval(parse(text=functionname)))
      if(is(x,"try-error")) {
        vcat(0,as.character(attr(x,"condition")))
        return(x)
      } 
    } else {
      x <- eval(parse(text=functionname))
    }
    if(!is.list(x)) stop("Output of function \"",functionname,"\" is not list of two MAgPIE objects containing the values and corresponding weights!")
    if(!is.magpie(x$x)) stop("Output x of function \"",functionname,"\" is not a MAgPIE object!")
    if(!is.magpie(x$weight) && !is.null(x$weight)) stop("Output weight of function \"",functionname,"\" is not a MAgPIE object!")
    if(!is.null(x$weight)) {
      if(nyears(x$x)!=nyears(x$weight) && nyears(x$weight)!=1) stop("Number of years disagree between data and weight of function \"",functionname,"\"!")
      if(nyears(x$weight)==1) getYears(x$weight) <- NULL
    }
    x$package <- attr(functionname,"pkgcomment")
    save(x,file=tmppath,compress = "xz")
    Sys.chmod(tmppath, mode = "0777", use_umask = TRUE)
  }
  
  # read and check x$isocountries value which describes whether the data is in
  # iso country resolution or not (affects aggregation and certain checks)
  if(is.null(x$isocountries)) {
    if(nregions(x$x)==1 && getRegions(x$x)=="GLO") {
      x$isocountries <- FALSE 
    } else {
      x$isocountries <- TRUE
    }
  }
  if(!is.logical(x$isocountries)) stop("x$isocountries must be a logical!")
  
  # read and check x$mixed_aggregation value which describes whether the data is in
  # mixed aggregation (weighted mean mixed with summation) is allowed or not
  if(is.null(x$mixed_aggregation)) x$mixed_aggregation <- FALSE
  if(!is.logical(x$mixed_aggregation)) stop("x$mixed_aggregation must be a logical!")
  
  # check for that aggregation=FALSE if x$isocountries in data is FALSE
  if(!x$isocountries) {
    if(aggregate!=FALSE) {
      stop("Aggregation is not available for calculations which do not return ISO-country data (x$isocountries=FALSE).")
    }
  }
  
  #check that data is returned for ISO countries except if x$isocountries=FALSE
  if(x$isocountries) {
    iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "madrat"),row.names=NULL)
    iso_country1<-as.vector(iso_country[,"x"])
    names(iso_country1)<-iso_country[,"X"]
    isocountries <- sort(iso_country1)
    datacountries <- sort(getRegions(x$x))
    if(length(isocountries)!=length(datacountries)) stop("Wrong number of countries returned by ",functionname,"!")
    if(any(isocountries!=datacountries)) stop("Countries returned by ",functionname," do not agree with iso country list!")
    if(!is.null(x$weight)) {
      if(nregions(x$weight)>1){
        weightcountries <- sort(getRegions(x$weight))
        if(length(isocountries)!=length(weightcountries)) stop("Wrong number of countries in weight returned by ",functionname,"!")
        if(any(isocountries!=weightcountries)) stop("Countries in weight returned by ",functionname," do not agree with iso country list!")
      }
    }
  }  
  
  #perform additional checks
  if(!is.null(x$min)) {
    if(any(x$x<x$min, na.rm = TRUE)) vcat(0,"Data returned by ", functionname," contains values smaller than the predefined minimum (min = ",x$min,")")
  }
  if(!is.null(x$max)) {
    if(any(x$x>x$max, na.rm = TRUE)) vcat(0,"Data returned by ", functionname," contains values greater than the predefined maximum (max = ",x$max,")")
  }
  if(na_warning) if(anyNA(x$x)) vcat(0,"Data returned by ", functionname," contains NAs")
  if(any(is.infinite(x$x))) vcat(0,"Data returned by ", functionname," contains infinite values")
  
  if(!is.null(years)){
    #check that years exist in provided data
    if(!all(as.integer(sub("y","",years)) %in% getYears(x$x,as.integer=TRUE))) stop("Some years are missing in the data provided by function ",functionname,"(", paste(years[!(as.integer(sub("y","",years))%in%getYears(x$x,as.integer=TRUE))],collapse=", "),")!")
    x$x <- x$x[,years,]
    if(!is.null(x$weight)) if(nyears(x$weight)>1) x$weight <- x$weight[,years,]
  }
  
  .prep_comment <- function(x,name,warning=NULL) {
    if(!is.null(x)) {
      x[1] <- paste0(" ",name,": ", x[1])
      if(length(x)>1) {
        x[2:length(x)] <- paste0(paste(rep(" ",3+nchar(name)),collapse=""), x[2:length(x)])
      }
    } else {
      if(!is.null(warning)) {
        vcat(0,warning)
        x <- paste0(" ",name,": not provided")
      }
    }
    return(x)
  }
  
  unit <- .prep_comment(x$unit,"unit",paste0('Missing unit information for data set "',type,'"!'))
  description <- .prep_comment(x$description,"description",paste0('Missing description for data set "',type,'"! Please add a description in the corresponding calc function!'))
  comment <- .prep_comment(getComment(x$x),"comment")
  origin <- .prep_comment(paste0(gsub("\\s{2,}"," ",paste(deparse(match.call()),collapse=""))," (madrat ",packageDescription("madrat")$Version," | ",x$package,")"),"origin")
  date <- .prep_comment(date(),"creation date")
  note <- .prep_comment(x$note,"note")
  
  glo_rel <- reg_rel <- read.csv(toolMappingFile("regional",getConfig("regionmapping")), as.is = TRUE, sep = ";")     
  glo_rel$RegionCode <- "GLO"
  
  # read and check x$aggregationFunction value which provides the aggregation function
  # to be used.
  if(is.null(x$aggregationFunction)) x$aggregationFunction <- "toolAggregate"
  if(!is.function(x$aggregationFunction) && !is.character(x$aggregationFunction)) stop("x$aggregationFunction must be a function!")
  
  # read and check x$aggregationArguments value which provides additional arguments
  # to be used in the aggregation function.
  if(is.null(x$aggregationArguments)) x$aggregationArguments <- list()
  if(!is.list(x$aggregationArguments)) stop("x$aggregationArguments must be a list of function arguments!")
  # Add base arguments to the argument list (except of rel, which is added later)
  x$aggregationArguments$x <- quote(x$x)
  if(!is.null(x$weight))  x$aggregationArguments$weight <- quote(x$weight)
  if(x$mixed_aggregation) x$aggregationArguments$mixed_aggregation <- TRUE
  
  if(aggregate==TRUE) {
    x$aggregationArguments$rel <- quote(reg_rel)
    x$x <- do.call(x$aggregationFunction,x$aggregationArguments)
  } else if (toupper(aggregate)=="GLO") {
    x$aggregationArguments$rel <- quote(glo_rel)
    x$x <- do.call(x$aggregationFunction,x$aggregationArguments)
  } else if(toupper(gsub("+","",aggregate,fixed = TRUE))=="REGGLO") {
    x$aggregationArguments$rel <- quote(glo_rel)
    tmp <- do.call(x$aggregationFunction,x$aggregationArguments)
    x$aggregationArguments$rel <- quote(reg_rel)
    x$x <- mbind(tmp,do.call(x$aggregationFunction,x$aggregationArguments))
  }
  
  if(!is.null(years)) {
    if(length(years)==1) getYears(x$x) <- NULL
  }
  if(!is.null(round)) {
    x$x <- round(x$x,round)
  }

  getComment(x$x) <- c(description,
                       unit,
                       note,
                       comment,
                       origin,
                       date)
  x$x<-clean_magpie(x$x)
  x$x<-updateMetadata(x$x,unit=x$unit,source=x$source,calcHistory="update",description=x$description,note=x$note,cH_priority=1)

  if(is.null(file) & append){
    vcat(0,"The parameter append=TRUE works only when the file name is provided in the calcOutput() function call.")
  }
  
  if(!is.null(file)) {
    if(!file.exists(getConfig("outputfolder"))) stop('Outputfolder "',getConfig("outputfolder"),'" does not exist!')
    if(grepl(".mif",file)==TRUE){
      if(!is.null(getYears(x$x))) { 
        write.report2(x$x,file=paste(getConfig("outputfolder"),file,sep="/"), unit=x$unit, append=append)
      } else {
        vcat(0,"Time dimension missing and data cannot be written to a mif-file. Skip data set!")
      }
    } else {
      write.magpie(x$x,file_folder=getConfig("outputfolder"),file_name=file, mode="777")
    }
  }
  if(supplementary) {
    return(x)
  } else {
    return(x$x)
  }
}
