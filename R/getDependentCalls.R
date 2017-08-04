multiplecallsinline <- function(line, checkstringi, type="calcOutput"){
  if(str_count(string = checkstringi, pattern = type)>1 &
     str_count(string = checkstringi, pattern =";" )>0){
    newStrings <- grep(pattern = type,x = str_split(string = checkstringi, pattern=";")[[1]],value = TRUE, perl = TRUE)
    return(list(newStrings, rep(line, times=length(newStrings) )))
  }
  return(list(checkstringi, line))
}



checkifcomplete <- function(i,lines, checkstring, fullfun ){
  if(str_count(checkstring[i], "\\(")!=str_count(checkstring[i], "\\)") |
     str_count(checkstring[i], "\\[")!=str_count(checkstring[i], "\\]")){
    checkstring[i] <- paste0(checkstring[i], fullfun[lines[i]+1])
      lines[i] <- lines[i]+1
     checkstring[i] <- checkifcomplete(i = 1, checkstring = checkstring[i],lines =lines[i], fullfun = fullfun)
  }
  checkstring[i] <- str_trim(string=gsub(pattern = "\\s{2,}", replacement = " ", x = checkstring[i]), side="both")
  return(checkstring[i])
}

getCallsOnly <- function(callchar, type="calcOutput"){

   callchar <- gsub(pattern = paste0(".*(?=",type, ")"), replacement="", x = callchar, perl=TRUE)
  if(str_count(callchar, "\\(")==str_count(callchar, "\\)")){
     if(str_count(callchar, ";")>0 & str_count(callchar, type)==1){
      callchar <- str_split(string = callchar, pattern = ";")
      callchar <- grep(pattern = type, x = callchar[[1]],value = TRUE , perl=TRUE)
    }
    value <- str_match(string=callchar, pattern=paste0("(?<=",type,"\\().+(?=\\))"))
    if(str_count(string = value, pattern = "\\)")>0){
      value <- sub(pattern = "\\).*", replacement = "", x = value)
    }
    return(paste0(type,"(",as.character(value),")"))
  }
  else{
     return(paste(str_split(string = callchar, pattern = "\\)")[[1]][1:str_count(callchar, "\\(")],")", collapse = ""))
  }
}

getType <- function(callstring,type="calcOutput" ){
  typef <- str_match(string= callstring, pattern =paste0("(?<=",type,"\\(\")\\w+(?=\")"))
  if(is.na(typef)) typef <-str_match(string = callstring, pattern=paste0("(?<=",type,"\\(type\\s{0,100}=\\s{0,100}\")\\w+(?=\")" ))
   return(typef)
}
dependentcalls <- function(type,prefix, years, ...){
  
  x <-  calccalls <- readcalls <-   types <- typesr <-  arguments <- argumentsr <- NULL
  functionname <- prepFunctionName(type=type, prefix=prefix)
  functionname[1] <- sub("\\(.*\\)", "", functionname[1], perl=TRUE)
  x <- deparse(eval(parse(text=functionname)))
  
  calccalls <- grep(pattern="calcOutput", x=x, value=TRUE, perl=TRUE)
  readcalls <- grep(pattern="readSource", x=x, value=TRUE, perl=TRUE)
  lines <- grep(pattern="calcOutput", x=x,  perl=TRUE)
  linesr <- grep(pattern="readSource", x=x,  perl=TRUE)
    callssep <- mapply(FUN = multiplecallsinline, line=lines, checkstringi=calccalls , SIMPLIFY = TRUE)
    readsep <- mapply(FUN = multiplecallsinline, line=linesr, checkstringi=readcalls, MoreArgs = list(type="readSource") , SIMPLIFY = TRUE)
  if(any(lengths(callssep)>1)){
    calccalls <- unlist(callssep[seq(from=1,to=length(callssep)-1, by=2)])
    lines <- unlist(callssep[seq(from=2,to=length(callssep), by=2)])
  }
  if(any(lengths(readsep)>1)){
    readcalls <- unlist(readsep[seq(from=1,to=length(readsep)-1, by=2)])
    linesr <- unlist(readsep[seq(from=2,to=length(readsep), by=2)])
  }
     if(!identical(calccalls, character(0))){
  calccalls <- sapply(X = c(1:length(calccalls)),FUN = checkifcomplete, lines=lines, checkstring=calccalls, fullfun=x)
  calccalls <- sapply(X = calccalls, FUN = getCallsOnly, USE.NAMES = FALSE)
  types <- sapply(X = calccalls, FUN = getType, USE.NAMES = FALSE)  
  arguments <-  as.vector(str_match(string=calccalls, pattern="(?<=calcOutput\\().+(?=\\))")) 
     }
    if(!identical(readcalls, character(0))){
  readcalls <- sapply(X = c(1:length(readcalls)),FUN = checkifcomplete, lines=linesr, checkstring=readcalls, fullfun=x)
  readcalls <- sapply(X = readcalls, FUN = getCallsOnly,type="readSource", USE.NAMES = FALSE)
  typesr <- sapply(X=readcalls, FUN=getType, type="readSource", USE.NAMES = FALSE) 
  argumentsr <- as.vector(str_match(string=readcalls, pattern="(?<=readSource\\().+(?=\\))"))
   }
return(list(calcOutputcalls=calccalls,readSourceCalls=readcalls))
}

#' @title getDependentCalls
#' @description Function to retrieve all calls of calcOutput and redSource
#' inside a calcOutput or readSource call.
#' @param type Type for which the calls it depends on should be retrieved
#' @param ... additional arguments passed on
#' @return a list containing n lists depending on the number of Calculations available for \code{type}.
#' Each list contains the associated calls as character vectors
#'
#' @author Stephen Wirth, Jan Philipp Dietrich
#' @examples
#' getDependentCalls("FAO")
#' getDependentCalls("fullMAGPIE")
#' @importFrom stringr str_match str_count str_trim str_split
#' @export
getDependentCalls <- function(type, ...){
  #@TODO: Are all Exceptions and possibility covered?
  
  prefixes <- c(calc=FALSE, red=FALSE, convert=FALSE, correct=FALSE, full=FALSE)
  vals <- list(calc=NULL, read=NULL, convert=NULL, correct=NULL, full=NULL)
  if(type %in% getCalculations(prefix="calc")$type) {
    prefixes["calc"]=TRUE
    vals$calc <- dependentcalls(type = type, prefix = "calc", ...)
  }
  if(type %in% getCalculations(prefix="read")$type){
    prefixes["read"]=TRUE
    vals$read <- dependentcalls(type = type, prefix = "read", ...)
  }
  if(type %in% getCalculations(prefix="convert")$type) {
    prefixes["convert"]=TRUE
    vals$convert <- dependentcalls(type = type, prefix = "convert", ...)
  }
  if (type %in% getCalculations(prefix="correct")$type){
    prefixes["correct"]=TRUE
   vals$correct <- dependentcalls(type = type, prefix = "correct", ...)
  }
  if (type %in% getCalculations(prefix="full")$type){
    prefixes["full"]=TRUE
    vals$full <- dependentcalls(type = type, prefix = "full", ...)
  }
  return(vals[names(prefixes)[prefixes]])
 
}