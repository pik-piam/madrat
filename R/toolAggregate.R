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
#' weighting column to NA.
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
#' @param rev function revision. rev=1 is the original version, rev=2 contains a
#' more efficient algorithm for calculations with weights
#' @return the aggregated data in magclass format
#' @author Jan Philipp Dietrich, Ulrich Kreidenweis
#' @export
#' @importFrom magclass wrap ndata fulldim clean_magpie mselect setCells getCells mbind setComment getNames getNames<- is.magpie getComment getComment<- dimCode getYears getRegionList as.magpie getItems 
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

toolAggregate <- function(x, rel, weight=NULL, from=NULL, to=NULL, dim=1, partrel=FALSE, negative_weight="warn", rev=2) {
  
  if(!is.null(weight) & rev>=2) {
    if(negative_weight!="allow" & any(weight<0)) {
      if(negative_weight=="warn") {
        warning("Negative numbers in weight. Dangerous, was it really intended?")
      } else {
        stop("Negative numbers in weight. Weight should be positive!")
      }
    }
    weight2 <- 1/(toolAggregate(weight, rel, from=from, to=to, dim=dim, partrel=partrel) + 10^-100)
    comment <- getComment(x)
    if(setequal(getItems(weight, dim=dim), getItems(x, dim=dim))) {
      out <- toolAggregate(x*weight,rel, from=from, to=to, dim=dim, partrel=partrel)*weight2
    } else if(setequal(getItems(weight2, dim=dim), getItems(x, dim=dim))) {
      out <- toolAggregate(x*weight2,rel, from=from, to=to, dim=dim, partrel=partrel)*weight
    } else {
      stop("Weight does not match data")
    }
    getComment(out) <- c(comment,paste0("Data aggregated (toolAggregate): ",date()))
    return(out)
  }  
  
  .getAggregationMatrix <- function(rel,from=NULL,to=NULL) {
    
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
  
  .toolScale <- function(x,rel,weight,dim=1,comment=NULL) {
    .calcweightmatrix <- function(weight,rel) {
    .tmp <- function(w,rel) {
      #return matrix if weights are all 0 or all NA (meaning that data should be summed up)
      if(all(is.na(w))) return(rel)
      if(any(is.na(w))) stop("Some but not all weights for a single data element are NA. You either have to provide weights for all spatial units or have to set all weights to NA (indicating that the data should be summed up). A mixture of NAs and values is not allowed!")
      if(any(is.infinite(w))) stop("Some elements of the weighting matrix are Inf. Infinite weights are not allowed!")
      if(all(w==0)) return(rel)
      w <- as.numeric(w)
      y <- as.numeric(rel %*% w)
      if(any(y==0)) {
        w <- w + 10^-100 
        y <- as.numeric(rel %*% w)
      }
      if(length(y)>10000 | length(w)>10000) {
        return(as.matrix(diag.spam(1/y,length(y)) %*% rel %*% diag.spam(w,length(w))))            
      } else {
        return(diag(1/y,length(y)) %*% rel %*% diag(w,length(w)))    
      }
    } 
    if(dim(weight)[dim]!=max(dim(rel)))stop("Weight and aggregation matrix do not agree in size! (weight = ",dim(weight)[dim],", aggregation matrix = ",max(dim(rel)),")")
    if(dim(rel)[1]>dim(rel)[2]) {
      rel <- t(rel)
      #reorder weight based on column names of relation matrix if available
      if(!is.null(rownames(rel))) if(any(colnames(rel)!=dimnames(weight)[[dim]])) {
        if(dim==1) weight <- weight[colnames(rel),,]
        if(dim==2) weight <- weight[,colnames(rel),]
        if(dim==3) weight <- weight[,,colnames(rel)]
      }
      #calculate all weighted aggregation matrices
      nd <- which(1:3!=dim)
      return(wrap(array(apply(weight,nd,.tmp,rel),dim=c(dim(rel),dim(weight)[nd]),dimnames=list(dimnames(rel)[[1]],dimnames(rel)[[2]],dimnames(weight)[[nd[1]]],dimnames(weight)[[nd[2]]])),map=list(2,1,3,4)))  
    } else { 
      #reorder weight based on column names of relation matrix if available
      if(!is.null(rownames(rel))) if(any(colnames(rel)!=dimnames(weight)[[dim]])) {
        if(dim==1) weight <- weight[colnames(rel),,]
        if(dim==2) weight <- weight[,colnames(rel),]
        if(dim==3) weight <- weight[,,colnames(rel)]
      }
      #calculate all weighted aggregation matrices
      nd <- which(1:3!=dim)
      return(array(apply(weight,nd,.tmp,rel),dim=c(dim(rel),dim(weight)[nd]),dimnames=list(dimnames(rel)[[1]],dimnames(rel)[[2]],dimnames(weight)[[nd[1]]],dimnames(weight)[[nd[2]]])))
    }
  }
    out <- list()
    w <- .calcweightmatrix(weight,rel)
    nd <- which(1:3!=dim)
    
    if(dim(w)[4]>1 & dim(w)[4]!=ndata(x)) {
      #check for possible name collisions in data dimension
      matches <- sapply(dimnames(w)[[4]],grepl,fulldim(x)[[2]])
      if(any(colSums(matches)>1)) {
        similar_subdimensions <- TRUE
        x <- clean_magpie(x,what = "sets")
        tmp <- sapply(fulldim(x)[[2]],setequal,dimnames(w)[[4]])
        subdim2use <- names(tmp)[tmp]
        if(length(subdim2use)>1) stop("Unclear mapping of weight to data subdimension!")
        if(length(subdim2use)==0) stop("Could not find data subdimension which matches the given data!")
      } else {
        similar_subdimensions <- FALSE      
      }
    } else {
      similar_subdimensions <- FALSE
    }
  
    for(y in 1:dim(w)[3]) {
      tmp <- list()
      if(dim(w)[3]==1) {
        if(dim==1) i1 <- 1:dim(x)[2]
        if(dim==2) i1 <- 1:dim(x)[1]
        if(dim==3) i1 <- 1:dim(x)[1]
      } else {
        i1 <- y
      }
      for(d in 1:dim(w)[4]) {      
        if(dim(w)[4]==1) {
          if(dim==1) i2 <- 1:dim(x)[3]
          if(dim==2) i2 <- 1:dim(x)[3]
          if(dim==3) i2 <- 1:dim(x)[2]
        } else {
          i2 <- dimnames(w)[[4]][d]
        }
        if(dim==1) {
          if(similar_subdimensions) {
            selectlist <- list()
            selectlist[[subdim2use]] <- i2
            tmp[[d]] <- toolAggregate(mselect(x[,i1,],selectlist),array(w[,,y,d],dim=dim(w)[1:2],dimnames=dimnames(w)[1:2]), dim=1)
          } else {
            tmp[[d]] <- toolAggregate(x[,i1,i2],array(w[,,y,d],dim=dim(w)[1:2],dimnames=dimnames(w)[1:2]), dim=1)
          }
        }
        if(dim==3) tmp[[d]] <- setCells(toolAggregate(x[i1,i2,],array(w[,,y,d],dim=dim(w)[1:2],dimnames=dimnames(w)[1:2]), dim=3),getCells(x[y,1,1])) 
        if(dim==2) {
          if(similar_subdimensions) {
            selectlist <- list()
            selectlist[[subdim2use]] <- i2
            tmp[[d]] <- setCells(toolAggregate(mselect(x[i1,,],selectlist),array(w[,,y,d],dim=dim(w)[1:2],dimnames=dimnames(w)[1:2]), dim=2),getCells(x[y,1,1]))
          } else {
            tmp[[d]] <- setCells(toolAggregate(x[i1,,i2],array(w[,,y,d],dim=dim(w)[1:2],dimnames=dimnames(w)[1:2]), dim=2),getCells(x[y,1,1]))
          }
        }
      }
      out[[y]] <- mbind(tmp)
    }  
    return(setComment(mbind(out),c(comment,paste0("Data aggregated (toolAggregate): ",date()))))
  }

  .expand_rel_weight <- function(rel,names,weight,dim){
    #Expand rel matrix to full dimension if rel is only provided for a subdimension
    
    if(round(dim)==dim | suppressWarnings(all(colnames(rel)==names))) {
      #return rel if nothing has to be done
      return(list(rel=rel,weight=weight))
    } 
      
    if(dim<3) stop("Subdimensions of spatial or temporal dimension are currently not supported!")
    
    subdim <- round((dim-floor(dim))*10)
    maxdim <- nchar(gsub("[^\\.]","",names[1])) + 1
  
    search <- paste0("^(",paste(rep("[^\\.]*\\.",subdim-1),collapse=""),")([^\\.]*)(",paste(rep("\\.[^\\.]*",maxdim-subdim),collapse=""),")$")
    onlynames <- unique(sub(search,"\\2",names))
    
    if(length(setdiff(colnames(rel),onlynames))>0) stop("The provided mapping contains entries which could not be found in the data: ",paste(setdiff(colnames(rel),onlynames),collapse=", "))
    if(length(setdiff(onlynames,colnames(rel)))>0) stop("The provided data set contains entries not covered by the given mapping: ",paste(setdiff(onlynames,colnames(rel)),collapse=", "))
    
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
    
    #reorder and possibly filter entries in new_rel so that it fits names
    new_rel <- new_rel[,names]
    
    if(is.null(weight)) return(list(rel=new_rel,weight=weight))
    
    if(ncol(new_rel)>=nrow(new_rel)) {
      requirednames <- colnames(new_rel)  
    } else {
      requirednames <- rownames(new_rel)
    }
    
    #weight is already related to full dimensionality
    if(all(requirednames %in% getNames(weight))) return(list(rel=new_rel,weight=weight))
    
    new_weight <- weight[,,rep(1,length(requirednames))]
    getNames(new_weight) <- requirednames
    for(n in getNames(weight)) new_weight[,,n] <- weight[,,n]
    
    return(list(rel=new_rel,weight=new_weight))  
  }

  if(!is.magpie(x)) {
    stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  }
  comment <- getComment(x)
  if(!is.numeric(rel) & !("spam" %in% class(rel))) {
    rel <- .getAggregationMatrix(rel,from=from,to=to) 
  }

  #translate dim to dim code
  dim <- dimCode(dim,x,missing=="stop")
  

  ## allow the aggregation, even if not for every entry in the initial dataset there is a respective one in the relation matrix
  if (partrel){
    datnames <- fulldim(x)[[2]][[ifelse(floor(dim)<=2, dim, (dim-floor(dim))*10+2)]]
    
    common <- intersect(datnames, colnames(rel))
    if(length(common)==0) stop("The relation matrix consited of no entry that could be used for aggregation")
    if(dim==1) x <- x[common,,]
    if(dim==2) x <- x[,common,]
    if(floor(dim)==3) x <- x[,,common]
    
    # datanames not in relnames
    noagg <- datnames[!datnames %in% colnames(rel)]
    if(length(noagg)>1) cat("The following entries were not aggregated because there was no respective entry in the relation matrix", noagg, "\n")
    
    rel <- rel[,common]
    rel <- subset(rel, subset=rowSums(rel)>0)
  }

  
  #make sure that rel and weight cover a whole dimension (not only a subdimension)
  #expand data if necessary
  #set dim to main dimension afterwards
  if(round(dim)!=dim) {
    tmp <- .expand_rel_weight(rel,getNames(x),weight,dim)
    rel <- tmp$rel
    weight <- tmp$weight
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
    if(dim==2) if(any(colnames(rel)!=getYears(x))) x <- x[,colnames(rel),]
    if(dim==3) if(any(colnames(rel)!=getNames(x))) x <- x[,,colnames(rel)]
  }
  
  #use function .toolScale if a weight is given
  if(!is.null(weight)) return(.toolScale(x,rel,weight,dim,comment))
  
  #Aggregate data
  matrix_multiplication <- function(y,x) {
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
  return(as.magpie(out,spatial=1,temporal=2))
}