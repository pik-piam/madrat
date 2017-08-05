#' Tool: ISOhistorical
#' 
#' This function expects a MAgPIE object with ISO country codes in the spatial
#' dimension. For this MAgPIE object the time of transition is calculated and
#' for each the historic time filled by using the mapping stored as
#' supplementary data in the madrat package. If you want to use a different
#' mapping please specify it in the argument mapping
#' 
#' 
#' @param m MAgPIE object with ISO country codes in the spatial dimension
#' @param mapping mapping of historical ISO countries to the standard ISO
#' country list. For the default setting (mapping=NULL) the mapping stored as
#' supplementary data in the madrat package is used.
#' @param additional_mapping vector or list of vectors to provide some specific
#' mapping, first the old country code, second the new country code and last
#' the last year of the old country, e.g. additional_mapping =
#' c("TTT","TTX","y1111") or additional_mapping =
#' list(c("TTT","TTX","y1111"),c("TTT","TTY","y1111"))
#' @param overwrite if there are already historical data in the data source for
#' years that are calculated in this function they will not be overwritten by
#' default. To overwrite all data (e.g. if there are meaningless "0") choose
#' overwrite=TRUE
#' @return A MAgPIE object with spatial entries for each country of the
#' official ISO code country list. Historical time is filled up, old countries
#' deleted
#' @author Lavinia Baumstark
#' 
#' @importFrom magclass getRegions getYears setYears
#' 
#' @export
toolISOhistorical <- function(m,mapping=NULL,additional_mapping=NULL,overwrite=FALSE){
  
  # m is magpie object, has to contain absolute values
  
  # mapping of historical countries and regions to the standard ISO-Country-List 
  #            and last year of existence of the historical countries
  if(is.null(mapping)){
    mapping <- read.csv2(system.file("extdata","ISOhistorical.csv",package = "madrat"),stringsAsFactors = F)
  } else if(is.character(mapping)) {
    mapping <- read.csv(mapping,sep=";",as.is=TRUE)
  }
  # add additional mapping, if provided
  if(!is.null(additional_mapping)){
    if(!is.list(additional_mapping)) {
      mapping <- rbind(mapping,additional_mapping)
    } else {
      for(elem in additional_mapping) {
        mapping <- rbind(mapping,elem)
      }
    }
  }
    
  # sort mapping(transitions) in historical order -> needed for the correct filling of data
  mapping <- mapping[order(mapping$lastYear),]
  # delete transitions from mapping which are not in the time horizon of m
#  print("The following transitions are ignorred as it exceeds the time horizon of the data",
#        subset(mapping, mapping$lastYear > max(intersect(mapping$lastYear,getYears(m))))        
#       )
  mapping <- subset(mapping, mapping$lastYear <= max(intersect(mapping$lastYear,getYears(m))) 
                           & mapping$lastYear >= min(intersect(mapping$lastYear,getYears(m))))
 

  # function to identify transitions that are in m
  .identifyTransitions <- function(m){
    tr <- list()
    # list of regions that are transition countries and in the data m 
    fromISO_m  <- intersect(mapping$fromISO,getRegions(m)) 
    # calculate number of transitions ntr 
    ntr <- 0
    h <- NULL
    fromISO_h <- list()
    for (i in 1:length(fromISO_m)){
      if (!length(mapping$toISO[mapping$fromISO==fromISO_m[i]])==1|i ==length(fromISO_m)){
        ntr <- ntr+1
        fromISO_h[[ntr]] <- c(h,fromISO_m[i])
        #print(fromISO_h)
      h <- NULL
      } else if (length(mapping$toISO[mapping$fromISO==fromISO_m[i]])==1){
        if(mapping$toISO[mapping$fromISO==fromISO_m[i]]!=mapping$toISO[mapping$fromISO==fromISO_m[i+1]]){
          ntr <- ntr+1
          fromISO_h[[ntr]] <- c(h,fromISO_m[i])
          #print(fromISO_h)
          h <- NULL
        } else {
          ntr <- ntr
          h <- c(h,fromISO_m[i])
          #print(h)
        }
      }
    } 
    # get informations for all transisitons
    for (i in 1:ntr){ 
      fromISO <- fromISO_h[[i]]
      toISO   <- mapping$toISO[mapping$fromISO==fromISO_h[[i]][1]]  
      # take the maximum year of m that is lower than the transition year
      fromY   <- max(getYears(m)[getYears(m)<=mapping$lastYear[mapping$fromISO==fromISO_h[[i]][1]][1]])
      # take the minimun of years that are later than fromY
      toY     <- min(getYears(m)[getYears(m)>fromY]) 
      tr[[i]] <- list(fromISO=fromISO,toISO=toISO,fromY=fromY,toY=toY)
    } 
  return(tr)
  }
  
  tr <- .identifyTransitions(m)
  vcat(2,"The following transitions are found in the data \n",paste(tr,collapse=", \n"))
  
  # loop over all transitions         
  for(a in tr) {
    # check if new regions of transition exists in m
    toISOmissing   <- all(is.element(a$toISO,getRegions(m)))
    if(toISOmissing==FALSE) stop("there is no data for the following new countrys: ", paste(a$toISO[which(!is.element(a$toISO,getRegions(m)))],collapse=", ") )
    
    # create transformation matrix
    ## time span where the data need to be adjusted
    sub_time <- getYears(m[,c(1:which(getYears(m)==a$fromY)),])
    # disaggregation of countries
    if(length(a$fromISO)==1){  
      m_tr <- toolAggregate(m[a$fromISO,sub_time,],mapping[is.element(mapping$toISO,a$toISO),c("fromISO","toISO")],weight=setYears(m[a$toISO,a$toY,],NULL))
    ## aggregation of countries
    } else{ 
      m_tr <- toolAggregate(m[a$fromISO,sub_time,],mapping[is.element(mapping$toISO,a$toISO),c("fromISO","toISO")],weight=NULL)
    }
    #print(m_tr)
    # fill data
    if(overwrite==TRUE){
      m[a$toISO,sub_time,] <- m_tr 
    }else{
      m[a$toISO,sub_time,][is.na(m[a$toISO,sub_time,])] <- m_tr[is.na(m[a$toISO,sub_time,])]
    }
  } # a in tr - transitions
  
  # delete old lines
  for(b in mapping$fromISO) {
    if(is.element(b,getRegions(m))) {
      m <- m[-which(getRegions(m)==b),,]
    }   
  }
  # print(m)
  
  return(m)
  
}







