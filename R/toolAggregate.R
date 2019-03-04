#' toolAggregate
#' 
#' (Dis-)aggregates a magclass object from one resolution to another based on a
#' relation matrix or mapping
#' 
#' Basically toolAggregate is doing nothing more than a normal matrix
#' multiplication which is taking into account the 3 dimensional structure of
#' MAgPIE objects. So, you can provide any kind of relation matrix you would
#' like. However, for easier usability it is also possible to provide weights
#' for a weighted (dis-)aggregation as a MAgPIE object. In this case rel must
#' be a 1-0-matrix or a mapping between both resolutions. The weight
#' needs to be provided in the higher spatial aggregation, meaning for
#' aggregation the spatial resolution of your input data and in the case of
#' disaggregation the spatial resolution of your output data. The temporal and
#' data dimension must be either identical to the resolution of the data set
#' that should be (dis-)aggregated or 1. If the temporal and/or data dimension
#' is 1 this means that the same transformation matrix is applied for all years
#' and/or all data columns. In the case that a column should be just summed up
#' instead of being calculated as a weighted average you either do not provide
#' any weight (than all columns are just summed up) or your set this specific
#' weighting column to NA and mixed_aggregation to TRUE.
#' 
#' @param x magclass object that should be (dis-)aggregated
#' @param rel relation matrix, mapping or csv file containing a mapping.
#' A mapping object should contain 2 columns in which each element of x
#' is mapped to the category it should belong to after (dis-)aggregation
#' @param weight magclass object containing weights which should be considered
#' for a weighted aggregation. The provided weight should only contain positive
#' values, but does not need to be normalized (any positive number>=0 is allowed). 
#' Please see the "details" section below for more information.
#' @param from Name of the first column to be used in rel if it is a
#' mapping (if not set the first or second column will be used). 
#' @param to Name of the second column to be used in rel if it is a
#' mapping (if not set the second or third column will be used). 
#' @param dim Specifying the dimension of the magclass object that should be
#' (dis-)aggregated. Either specified as an integer
#' (1=spatial,2=temporal,3=data) or if you want to specify a sub dimension
#' specified by name of that dimension or position within the given dimension
#' (e.g. 3.2 means the 2nd data dimension, 3.8 means the 8th data dimension).
#' @param partrel If set to TRUE allows that the relation matrix does contain
#' less entries than x and vice versa. These values without relation are lost
#' in the output.
#' @param negative_weight Describes how a negative weight should be treated. "allow"
#' means that it just should be accepted (dangerous), "warn" returns a warning and
#' "stop" will throw an error in case of negative values
#' @param mixed_aggregation boolean which allows for mixed aggregation (weighted 
#' mean mixed with summations). If set to TRUE weight columns filled with NA
#' will lead to summation.
#' @param verbosity Verbosity level of messages coming from the function: -1 = error, 
#' 0 = warning, 1 = note, 2 = additional information, >2 = no message
#' @return the aggregated data in magclass format
#' @author Jan Philipp Dietrich, Ulrich Kreidenweis
#' @export
#' @importFrom magclass wrap ndata fulldim clean_magpie mselect setCells getCells mbind setComment getNames getNames<- 
#' @importFrom magclass is.magpie getComment getComment<- dimCode getYears getYears<- getRegionList as.magpie getItems collapseNames updateMetadata
#' @importFrom spam diag.spam as.matrix
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' # create example mapping
#' mapping <- data.frame(from=getRegions(population_magpie),to=rep(c("REG1","REG2"),5))
#' mapping 
#' 
#' # run aggregation
#' toolAggregate(population_magpie,mapping)
#' # weighted aggregation
#' toolAggregate(population_magpie,mapping, weight=population_magpie)

toolAggregate <- function(x, rel, weight=NULL, from=NULL, to=NULL, dim=1, partrel=FALSE, negative_weight="warn", mixed_aggregation=FALSE, verbosity=1) {

  if(!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  
  comment <- getComment(x)
  #Special handling for when calcOutput calls do.call(x$aggregationFunction,x$aggregationArguments). More general solution maybe necessary.
  if (getOption("metadata_verbosity")==2) {
    if (as.character(sys.call())[1]=="toolAggregate")  calcHistory <- "update"
    else if (length(sys.calls())>1 && as.character(sys.call(-1))[1]=="toolAggregate")  calcHistory <- "copy"
    else {
      for (i in 1:length(sys.calls())) {
        if (as.character(sys.call(-i))[1]=="calcOutput") {
          if (all(get("reg_rel",envir=sys.frame(-i))==rel))  rel_calc <- "reg_rel"
          else  rel_calc <- "glo_rel"
          break
        }
      }
      calcHistory <- paste0("toolAggregate(x$x, ",rel_calc,", x$weight, dim=",dim,", mixed_aggregation=",mixed_aggregation,")")
    }
  }else  calcHistory <- "copy"
  
  if(!is.numeric(rel) & !("spam" %in% class(rel))) {
    .getAggregationMatrix <- function(rel,from=NULL,to=NULL) {
      
      if("tbl" %in% class(rel)){
        rel <- data.frame(rel)
      }
      if(!(is.matrix(rel) | is.data.frame(rel))) {
        if(!file.exists(rel)) stop("Cannot find given region mapping file!")
        rel <- read.csv(rel, as.is = TRUE, sep = ";")     
      }
      
      if(is.null(from)) {
        from <- ifelse(dim(rel)[2]==3,2,1)
      }
      if(is.null(to)) {
        to <- ifelse(dim(rel)[2]==3,3,2)
      }
      
      regions <- unique(rel[,to])
      countries <- unique(rel[,from])
      m <- matrix(data=0, nrow=length(regions),ncol=length(countries),dimnames=list(regions=regions,countries=countries))
      m[cbind(match(rel[,to],rownames(m)),match(rel[,from],colnames(m)))] <- 1
      if(is.numeric(to)) to <- dimnames(rel)[[2]][to]
      if(is.numeric(from)) from <- dimnames(rel)[[2]][from]
      names(dimnames(m)) <- c(to,from)
      return(m)
    }  
    rel <- .getAggregationMatrix(rel,from=from,to=to) 
  }

  #translate dim to dim code
  dim <- dimCode(dim,x,missing=="stop")
  

  ## allow the aggregation, even if not for every entry in the initial dataset there is a respective one in the relation matrix
  if (partrel){
    datnames <-  getItems(x,dim)
    
    common <- intersect(datnames, colnames(rel))
    if(length(common)==0) stop("The relation matrix consited of no entry that could be used for aggregation")
    if(floor(dim)==1) x <- x[common,,]
    if(floor(dim)==2) x <- x[,common,]
    if(floor(dim)==3) x <- x[,,common]
    
    # datanames not in relnames
    noagg <- datnames[!datnames %in% colnames(rel)]
    if(length(noagg)>1) vcat(verbosity, "The following entries were not aggregated because there was no respective entry in the relation matrix", noagg, "\n")
    
    rel <- rel[,common]
    rel <- subset(rel, subset=rowSums(rel)>0)
  }

  if(!is.null(weight)) {
    if(!is.magpie(weight)) stop("Weight is not a MAgPIE object, weight has to be a MAgPIE object!")
    if(anyNA(weight)) {
      if(!mixed_aggregation) {
        stop("Weight contains NAs which is only allowed if mixed_aggregation=TRUE!")
      } else {
        n <- length(getItems(weight,dim=dim))
        r <- dimSums(is.na(weight), dim=dim)
        if(!all(r %in% c(0,n))) stop("Weight contains columns with a mix of NAs and numbers which is not allowed!")
      }
    }
    if(nyears(weight)==1) getYears(weight) <- NULL
    weight <- collapseNames(weight)
    if(negative_weight!="allow" & any(weight<0, na.rm=TRUE)) {
      if(negative_weight=="warn") {
        warning("Negative numbers in weight. Dangerous, was it really intended?")
      } else {
        stop("Negative numbers in weight. Weight should be positive!")
      }
    }
    weight2 <- 1/(toolAggregate(weight, rel, from=from, to=to, dim=dim, partrel=partrel, verbosity=10) + 10^-100)
    if(mixed_aggregation) {
      weight2[is.na(weight2)] <- 1
      weight[is.na(weight)] <- 1
    }
    
    if(setequal(getItems(weight, dim=dim), getItems(x, dim=dim))) {
      out <- toolAggregate(x*weight,rel, from=from, to=to, dim=dim, partrel=partrel)*weight2
    } else if(setequal(getItems(weight2, dim=dim), getItems(x, dim=dim))) {
      out <- toolAggregate(x*weight2,rel, from=from, to=to, dim=dim, partrel=partrel)*weight
    } else {
      if(partrel) {
        stop("Weight does not match data. For partrel=TRUE make sure that the weight is already reduced to the intersect of relation matrix and x!") 
      } else {
        stop("Weight does not match data")
      }
    }
    getComment(out) <- c(comment,paste0("Data aggregated (toolAggregate): ",date()))
    return(updateMetadata(out,x,unit="copy",calcHistory=calcHistory))
  }  else {
  
    #make sure that rel and weight cover a whole dimension (not only a subdimension)
    #expand data if necessary
    #set dim to main dimension afterwards
    if(round(dim)!=dim) {
      .expand_rel <- function(rel,names,dim){
        #Expand rel matrix to full dimension if rel is only provided for a subdimension
        
        if(round(dim)==dim | suppressWarnings(all(colnames(rel)==names))) {
          #return rel if nothing has to be done
          return(rel)
        }
        
        if (2<dim & dim<3)  stop("Subdimensions of temporal dimension are currently not supported!")
        else if (1.2<dim & dim<2)  stop("Only 2 subdimensions are currently supported for the spatial dimension!")
        else if (1<dim & dim<4) {
          if (dim < 2)  names <- getCells(x)
          subdim <- round((dim-floor(dim))*10)
          maxdim <- nchar(gsub("[^\\.]","",names[1])) + 1
          
          search <- paste0("^(",paste(rep("[^\\.]*\\.",subdim-1),collapse=""),")([^\\.]*)(",paste(rep("\\.[^\\.]*",maxdim-subdim),collapse=""),")$")
          onlynames <- unique(sub(search,"\\2",names))
          
          if(length(setdiff(colnames(rel),onlynames))>0) {
            if (length(setdiff(rownames(rel),onlynames))>0) {
              stop("The provided mapping contains entries which could not be found in the data: ",paste(setdiff(colnames(rel),onlynames),collapse=", "))
            }else  rel <- t(rel)
          }else if(length(setdiff(onlynames,colnames(rel)))>0) {
            if (length(setdiff(onlynames,rownames(rel)))>0) {
              stop("The provided data set contains entries not covered by the given mapping: ",paste(setdiff(onlynames,colnames(rel)),collapse=", "))
            }else  rel <- t(rel)
          }
          
          tmp <- unique(sub(search,"\\1#|TBR|#\\3",names)) 
          additions <- strsplit(tmp,split="#|TBR|#",fixed=TRUE)
          cnames <- NULL
          rnames <- NULL
          for(i in 1:length(additions)) {
            if(is.na(additions[[i]][2])) additions[[i]][2] <- ""
            cnames <- c(cnames,paste0(additions[[i]][1],colnames(rel),additions[[i]][2]))
            rnames <- c(rnames,paste0(additions[[i]][1],rownames(rel),additions[[i]][2]))
          }
          
          new_rel <- matrix(0,nrow=length(rnames),ncol=length(cnames),dimnames=list(rnames,cnames))
          
          for(i in 1:length(additions)) {
            new_rel[1:nrow(rel)+(i-1)*nrow(rel),1:ncol(rel)+(i-1)*ncol(rel)] <- rel
          }
          return(new_rel[,names])
        }
      }
      rel <- .expand_rel(rel,getNames(x),dim)
      dim <- round(floor(dim))
    }
    
    if(dim(x)[dim]!=dim(rel)[2]){
      if(dim(x)[dim]!=dim(rel)[1]) {
        stop("Relation matrix has in both dimensions a different number of entries (",dim(rel)[1],", ",dim(rel)[2],") than x has cells (",dim(x)[dim],")!")
      } else {
        rel <- t(rel)
      }
    }
    
    #reorder MAgPIE object based on column names of relation matrix if available
    if(!is.null(colnames(rel))) {
      if(dim==1) if(any(colnames(rel)!=getCells(x))) x <- x[colnames(rel),,]
      else if(dim==2) if(any(colnames(rel)!=getYears(x))) x <- x[,colnames(rel),]
      else if(dim==3) if(any(colnames(rel)!=getNames(x))) x <- x[,,colnames(rel)]
    }
    
    #Aggregate data
    matrix_multiplication <- function(y,x) {
      if(any(is.infinite(y))) {
        #Special Inf treatment to prevent that a single Inf in x
        #is setting the full output to NaN (because 0*Inf is NaN)
        #Infs are now treated in a way that anything except 0 times Inf
        #leads to NaN, but 0 times Inf leads to NaN
        for(i in c(-Inf,Inf)) {
          x[,y==i][x[,y==i]!=0] <- i
          y[y==i] <- 1
        }
      }
      if(any(is.na(y))) {
        #Special NA treatment to prevent that a single NA in x
        #is setting the full output to NA (because 0*NA is NA)
        #NAs are now treated in a way that anything except 0 times NA
        #leads to NA, but 0 times NA leads to 0
        x[,is.na(y)][x[,is.na(y)]!=0] <- NA
        y[is.na(y)] <- 0
      }
      return(x%*%y)   
    }
    out <- apply(x, which(1:3!=dim),matrix_multiplication,rel)
    if(length(dim(out))==2) out <- array(out,dim=c(1,dim(out)),dimnames=c("",dimnames(out)))
     
    #Write dimnames of aggregated dimension
    if(!is.null(rownames(rel))) {
      reg_out <- rownames(rel)
    } else if(dim==1) {
      reg_in <- getRegionList(x)
      reg_out <- factor(round(rel %*% as.numeric(reg_in)/(rel %*% 
          rep(1, dim(rel)[2]))))
      levels(reg_out) <- levels(reg_in)
    } else {
      stop("Missing dimnames for aggregated dimension")
    }
    if(!any(grepl("\\.",reg_out))) {
      if(anyDuplicated(reg_out)) reg_out <- paste(reg_out,1:dim(out)[1],sep=".")
    }
    
    dimnames(out)[[1]] <- reg_out
    
    if(dim==2) out <- wrap(out,map=list(2,1,3))
    if(dim==3) out <- wrap(out,map=list(2,3,1))
    
    getComment(out) <- c(comment,paste0("Data aggregated (toolAggregate): ",date()))
    out <- as.magpie(out,spatial=1,temporal=2)
    return(updateMetadata(out,x,unit="copy",calcHistory=calcHistory))
  }
}
