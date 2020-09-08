#' getMadratGraph
#' 
#' Function returns the madrat graph of all linkages of full, calc and read functions of the given madrat 
#' based packages
#' 
#' 
#' @param packages A character vector with packages for which the available Sources/Calculations should be returned
#' @param globalenv Boolean deciding whether sources/calculations in the global environment should be included or not
#' @return A data frame with 4 columns: from (source function), from_package (package the source function originates from),
#' to (function which is using the source), to_package (package of the using function)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCalculations}}, \code{\link{getConfig}}
#' @importFrom stringi stri_match_all_regex
#' @export

getMadratGraph <- function(packages=getConfig("packages"), globalenv=getConfig("globalenv")) {
  
  # extract function pool
  classes <- c("read","calc","full","tool")
  fpool <- do.call(rbind,lapply(classes,getCalculations, packages=packages, globalenv=globalenv))
  
  # check for duplicates
  fpool$fname <- sub("^.*:::","",fpool$call)
  duplicated <- duplicated(fpool$fname, fromLast=TRUE)
  if(any(duplicated)) {
    base::warning("Duplicate entries found for ",paste(fpool$fname[duplicated],collapse=", "),"! Last entry will be used!")
    fpool <- fpool[!duplicated,]
  }
  
  # read in source code
  extractCode <- function(x) {
    out <- deparse(eval(parse(text=x)))
    pattern <- "(^|::)read"
    if(grepl(pattern,x)) {
      extras <- c("download","convert","correct")
      for(e in extras) {
        tmp <- try(deparse(eval(parse(text=sub(pattern,paste0("\\1",e),x)))),silent=TRUE)
        if(!("try-error" %in% class(tmp))) out <- c(out,tmp)
      }
    }
    return(paste(out,collapse=" "))
  }
  code <- sapply(fpool$call,extractCode)

  # merge download-, convert-, correct- and read-calls just as read-calls
  
  fpool$fname <- sub("^(correct|convert|download)","read", fpool$fname)
  fpool$call <- sub("(^|:::)(correct|convert|download)","\\1read", fpool$call)
  #names(code) <- sub("(^|:::)(correct|convert|download)","\\1read", names(code))
  
  # extract read/calc calls
  pattern <- "(readSource|calcOutput)\\( *([^=\"',]*=|) *(\"|')?([^\"',]*)[\"']?"
  matches <- stri_match_all_regex(code,pattern, omit_no_match = TRUE)
  names(matches) <- names(code)
  tmpfun <- function(x,l) {
    if(length(l[[x]])==0) return(NULL)
    out <- cbind(x,paste0(substring(l[[x]][,2],1,4),l[[x]][,5]),l[[x]])
    return(out[,c(1:4,6:7)])
  }
  out <- lapply(names(matches),tmpfun,matches)
  out <- do.call(rbind,out)
  # clean up output
  colnames(out) <- c("to","from","raw","class","quote","type")
  out <- as.data.frame(out, stringsAsFactors=FALSE)
    
  # extract tool calls
  pattern <- "tool([^( ]*)\\("
  matches <- stri_match_all_regex(code,pattern, omit_no_match = TRUE)
  names(matches) <- names(code)
  tmpfun <- function(x,l) {
    if(length(l[[x]])==0) return(NULL)
    out <- cbind(x,l[[x]])
    return(out)
  }
  out2 <- lapply(names(matches),tmpfun,matches)
  out2 <- do.call(rbind,out2)
  # clean up output
  colnames(out2) <- c("to","from","type")
  out2 <- as.data.frame(out2, stringsAsFactors=FALSE)
  out2$from <- sub("(","",out2$from,fixed=TRUE)
  out2$class <- "tool"
  
  
  # set from info to NA for cases in which call statement could not be read properly
  out$from[is.na(out$quote)] <- NA
  out <- unique(rbind(out[,c("from","to")],out2[,c("from","to")]))
  out$from_package <- as.character(fpool$package[match(out$from,fpool$fname)])
  out$to_package <- sub(":::.*$","",out$to)
  out$to <- sub("^.*:::","",out$to) 
  
  fromNA <- is.na(out$from)
  if(any(fromNA)) {
    out$from[is.na(out$from)] <- "UNKNOWN"
    out$from_package[fromNA]  <- "UNKNOWN"
    base::warning("Following functions contain read or calc statements which could not be identified: \n   ",
            paste(out$to[fromNA],collapse=", "),"\n  Please adress the type explicitly in the call to allow",
            " for proper detection, e.g. readSource(\"MySource\")")
  }
  
  from_packageNA <- is.na(out$from_package) & !fromNA
  if(any(from_packageNA)) {
    out$from_package[from_packageNA] <- "UNKNOWN"
    base::warning("Following functions could not be found in the scope of packages to be checked.: \n   ",
            paste0(out$from[from_packageNA],"->",out$to[from_packageNA],collapse=", "),"\n  Please make sure that they exist and adjust",
            " the scope of packages accordingly!")
  }
  # check for bidirectional package connections
  pkgdeps <- unique(out[c("from_package","to_package")])
  pkgdeps <- pkgdeps[pkgdeps$from_package!=pkgdeps$to_package & pkgdeps$from_package!="UNKNOWN",]
  if(nrow(pkgdeps)>2) {
    din <- paste0(pkgdeps$from_package,"-",pkgdeps$to_package)
    dout <- paste0(pkgdeps$to_package,"-",pkgdeps$from_package)  
    if(any(din %in% dout)) {
      p <- pkgdeps[din %in% dout,]
      hints <- NULL
      for(i in 1:nrow(p)) {
        tmp <- out[out$from_package==p$from_package[i] & out$to_package==p$to_package[i],]
        if(nrow(tmp)<5 & nrow(tmp)>0) hints <- paste0(hints,paste0(tmp$from,"->",tmp$to,collapse=", "), collapse=", ") 
      }
      base::warning("Bidirectional package dependencies detected: ",paste0(p$from_package,"->",p$to_package,collapse=", "),
              "\n  You might want to have a look at the following connections: ",hints)
    }
  }
  attr(out,"fpool") <- fpool
  return(out)
}