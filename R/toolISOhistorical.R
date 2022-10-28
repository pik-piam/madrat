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
#' supplementary data in the madrat package is used. If provided as file the
#' mapping needs to contain three columns "fromISO", "toISO" and "lastYear".
#' @param additional_mapping vector or list of vectors to provide some specific
#' mapping, first the old country code, second the new country code and last
#' the last year of the old country, e.g. additional_mapping =
#' c("TTT","TTX","y1111") or additional_mapping =
#' list(c("TTT","TTX","y1111"),c("TTT","TTY","y1111"))
#' @param overwrite if there are already historical data in the data source for
#' years that are calculated in this function they will not be overwritten by
#' default. To overwrite all data (e.g. if there are meaningless "0") choose
#' overwrite=TRUE
#' @param additional_weight optional weight to be used for regional disaggregation,
#' if not provided, the values of m in the "lastYear" are used as weight
#' @return A MAgPIE object with spatial entries for each country of the
#' official ISO code country list. Historical time is filled up, old countries
#' deleted
#' @author Lavinia Baumstark
#'
#' @importFrom magclass getItems getYears setYears
#' @importFrom stats setNames
#'
#' @export
toolISOhistorical <- function(m, mapping = NULL, additional_mapping = NULL, overwrite = FALSE, additional_weight = NULL) { # nolint
  
  # m is magpie object, has to contain absolute values
  
  # mapping of historical countries and regions to the standard ISO-Country-List
  #            and last year of existence of the historical countries
  if (is.null(mapping)) {
    mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"), stringsAsFactors = FALSE)
  } else if (is.character(mapping)) {
    mapping <- read.csv(mapping, sep = ";", as.is = TRUE)
  }
  # add additional mapping, if provided
  if (!is.null(additional_mapping)) {
    if (is.data.frame(additional_mapping) || !is.list(additional_mapping)) {
      mapping <- rbind(mapping, additional_mapping)
    } else {
      for (elem in additional_mapping) {
        mapping <- rbind(mapping, elem)
      }
    }
  }
  
  # sort mapping(transitions) in historical order -> needed for the correct filling of data
  mapping <- mapping[robustOrder(mapping$lastYear), ]
  mapping <- mapping[!duplicated(mapping), ] # make sure no repeats
  # delete transitions from mapping which are not in the time horizon of m
  mapping <- subset(mapping,
                    mapping$lastYear <= max(intersect(mapping$lastYear, getYears(m))) &
                      mapping$lastYear >= min(intersect(mapping$lastYear, getYears(m))))
  
  # function to identify transitions that are in m
  .identifyTransitions <- function(iM, dim = 1.1) {
    tr <- list()
    # list of regions that are transition countries and in the data iM
    fromISOM  <- intersect(mapping$fromISO, getItems(iM, dim = dim))
    # create matrix of possible transitions
    ptr <- NULL
    for (i in fromISOM) {
      # loop over number of different years of transition for one fromISOM-country
      for (l in seq_along(unique(mapping$lastYear[mapping$fromISO == i]))) {
        ptr <- rbind(ptr, c(i, unique(mapping$lastYear[mapping$fromISO == i])[l]))
      }
    }
    # sort again based on transition year if more than one transition exists
    if (length(ptr[, 1]) > 1) {
      ptr <- ptr[robustOrder(ptr[, 2]), ]
    }
    # calculate number of transitions ntr
    ntr <- 0
    h <- NULL
    fromISOYear <- list()
    for (i in seq_along(ptr[, 1])) {
      if (!length(mapping$toISO[mapping$fromISO == ptr[i, 1]]) == 1 || i == length(ptr[, 1])) {
        ntr <- ntr + 1
        fromISOYear[[ntr]] <- cbind(h, ptr[i, ])
        h <- NULL
      } else if (length(mapping$toISO[mapping$fromISO == ptr[i, 1]]) == 1) {
        to1 <- mapping$toISO[mapping$fromISO == ptr[i, 1]]
        to2 <- mapping$toISO[mapping$fromISO == ptr[i + 1, 1]]
        if (length(to1) != length(to2) || to1 != to2) {
          ntr <- ntr + 1
          fromISOYear[[ntr]] <- cbind(h, ptr[i, ])
          h <- NULL
        } else {
          h <- cbind(h, ptr[i, ])  # evtl ptr[i,1] für Vereinigungen
        }
      }
    }
    
    # collect information for all transisitons
    for (i in seq_len(ntr)) {
      fromISO <- fromISOYear[[i]][1, ]
      toISO   <- mapping$toISO[mapping$fromISO == fromISO[1] & mapping$lastYear == fromISOYear[[i]][2]]
      # take the maximum year of m that is lower than the transition year
      fromY   <- max(getYears(iM)[getYears(iM) <= fromISOYear[[i]][2]])
      # take the minimun of years that are later than fromY
      toY     <- min(getYears(iM)[getYears(iM) > fromY])
      tr[[i]] <- list(fromISO = fromISO, toISO = toISO, fromY = fromY, toY = toY)
    }
    return(tr)
  }
  
  tr <- .identifyTransitions(m)
  vcat(2, "The following transitions are found in the data \n", paste(tr, collapse = ", \n"))

  if (ndim(m, dim = 1) == 1) { # normal process
    
    # loop over all transitions
    for (a in tr) {
      # check if new regions of transition exists in m
      toISOmissing   <- !all(is.element(a$toISO, getItems(m, dim = 1.1)))
      if (toISOmissing) stop("there is no data for the following new countrys: ",
                             paste(a$toISO[which(!is.element(a$toISO, getItems(m, dim = 1.1)))], collapse = ", "))
      
      # create transformation matrix
      ## time span where the data need to be adjusted
      subTime <- getYears(m[, seq_len(which(getYears(m) == a$fromY)), ])
      # disaggregation of countries
      
      if (length(a$fromISO) == 1) {
        
        if (is.null(additional_weight)) {
          weight <- setYears(m[a$toISO, a$toY, ], NULL)
          if (anyNA(weight)) {
            weight[is.na(weight)] <- 0
            vcat(0, "Weight in toolISOhistorical contained NAs. Set NAs to 0!")
          }
        } else {
          if (!all(a$toISO %in% getItems(additional_weight, dim = 1))) {
            stop(paste0(
              "Invalid additional weight, missing countries: ",
              paste0(setdiff(a$toISO, getItems(additional_weight, dim = 1)), collapse = ", ")
            ))
          }
          weight <- additional_weight[a$toISO, , ]
        }
        
        mTr <- toolAggregate(m[a$fromISO, subTime, ],
                             mapping[is.element(mapping$toISO, a$toISO), c("fromISO", "toISO")], weight = weight,
                             negative_weight = "allow")
        ## aggregation of countries
      } else {
        mTr <- toolAggregate(m[a$fromISO, subTime, ],
                             mapping[is.element(mapping$toISO, a$toISO), c("fromISO", "toISO")], weight = NULL)
      }
      # fill data
      if (overwrite == TRUE) {
        m[a$toISO, subTime, ] <- mTr
      } else {
        m[a$toISO, subTime, ][is.na(m[a$toISO, subTime, ])] <- mTr[is.na(m[a$toISO, subTime, ])]
      }
    } # a in tr - transitions
    
    # delete old lines
    for (b in mapping$fromISO) {
      if (is.element(b, getItems(m, dim = 1.1))) {
        m <- m[-which(getItems(m, dim = 1.1) == b), , ]
      }
    }
  } else if (ndim(m, dim = 1) == 2) {
    # bilateral process needs to iterate over dim 1 and dim 2 due to potential presence of different transitions
    
    tr2 <- .identifyTransitions(m, dim = 1.2)
    vcat(2, "The following transitions are found in the data for dim 1.2 \n", paste(tr2, collapse = ", \n"))
  
 
 # function to fill in a specific dimension
  .histDim <- function(z, tr, mainDim, dims, overwrite) {

           secdDim <- setdiff(c(1.1, 1.2), mainDim)
           mainDimName <-dims[as.integer(gsub("1\\.", "", mainDim))]
           secdDimName <- dims[which(dims != mainDimName)]
           
      # check if new regions of transition exists in m
      toISOmissing <- !all(is.element(tr$toISO, getItems(z, dim = mainDim)))
      if (toISOmissing) {
        missingNew <- new.magpie(cells_and_regions =
                                   as.vector(outer(tr$toISO[which(!is.element(tr$toISO, getItems(z, dim = mainDim)))],
                                                   getItems(z, dim = secdDim), paste, sep = ".")),
                                 years = getItems(m, dim = 2),
                                 names = getItems(m, dim = 3),
                                 fill = 0)
        z <- mbind(z, missingNew)
        
        vcat(1, "Data was newly created and given 0 weight for the following new countrys in dim 1.1: ",
             getItems(missingNew, dim = mainDim))
      }

      # create transformation matrix
      ## time span where the data need to be adjusted
      subTime <- getYears(z[, seq_len(which(getYears(z) == tr$fromY)), ])
      # disaggregation of countries
      
      if (length(tr$fromISO) == 1) {
        
        if (is.null(additional_weight)) {

         weight <- setYears(z[setNames(list(c(tr$toISO)), mainDimName), tr$toY, ], NULL)
          # mapping needs to be subset to the exact combinations present in the weight for toolAggregate
          # if weight does not contain some of the combinations in m add it
          addR <- setdiff(getItems(z[setNames(list(c(tr$fromISO)), mainDimName), subTime, ], dim = secdDim),
                          getItems(weight, dim = secdDim))
          if (length(addR) > 0) {
            if(mainDim == 1.1) {
              addR <-  as.vector(outer(getItems(weight, dim = mainDim), addR, paste, sep = "."))
            } else if(mainDim == 1.2) {
              addR <- as.vector(outer(addR, getItems(weight, dim = mainDim), paste, sep = "."))
            }}
           #sometimes individual bilateral combination may be missing, leading to weights that don't sum to 1
          if(mainDim == 1.1){
          addInd <- setdiff(as.vector(outer(
                              tr$toISO,
                               getItems(z[setNames(list(c(tr$fromISO)), mainDimName), subTime, ], dim = secdDim),
                               paste, sep = ".")),
                            getItems(weight, dim = 1))
          } else if (mainDim == 1.2){
         addInd <- setdiff(as.vector(outer(
                               getItems(z[setNames(list(c(tr$fromISO)), mainDimName), subTime, ], dim = secdDim),
                               tr$toISO, paste, sep = ".")),
                            getItems(weight, dim = 1))
          }
          if(length(addInd) > 0){
            addR <- unique(c(addR, addInd))
          }
          if (length(addR) > 0){
            addMiss <- new.magpie(cells_and_regions =  addR,
                                  years = getItems(weight, dim = 2),
                                  names = getItems(weight, dim = 3),
                                  fill = 0)
            weight <- mbind(weight, addMiss)
          }
    
  
          if (anyNA(weight)) {
            weight[is.na(weight)] <- 0
            vcat(0, "Weight 1 in toolISOhistorical contained NAs. Set NAs to 0!")
          }
        } else {
          stop("additional_weight is not supported for bilateral data.")
        }
        
        bilatMapping <- mapping # mapping needs to be made bilateral
      if (mainDim == 1.1) {bilatMapping <- data.frame(toISO = as.vector(outer(bilatMapping$toISO, getItems(z, dim = secdDim), paste, sep = ".")),
                                   fromISO = as.vector(outer(bilatMapping$fromISO,
                                                             getItems(z, dim = secdDim), paste, sep = ".")))
      }else if (mainDim == 1.2) {bilatMapping <- data.frame(toISO = as.vector(outer(getItems(z, dim = secdDim), bilatMapping$toISO, paste, sep = ".")),
                                   fromISO = as.vector(outer(getItems(z, dim = secdDim), 
                                                           bilatMapping$fromISO, paste, sep = ".")))
      }
        
        zTr <- toolAggregate(z[setNames(list(c(tr$fromISO)), mainDimName), subTime, ],
                             bilatMapping[is.element(bilatMapping$toISO,               
                                         getItems(weight[setNames(list(c(getItems(z[setNames(list(c(tr$fromISO)), 
                                                                                  mainDimName), , ], dim = secdDimName))),  #nolint
                                                         secdDimName), , ], 
                                                  dim = 1)),
                                        c("fromISO", "toISO")],
                             from = "fromISO", to = "toISO",
                             dim = 1,
                             weight = weight[setNames(list(c(getItems(z[setNames(list(c(tr$fromISO)), mainDimName), , ], dim = secdDimName))), secdDimName), , ] + 1e-8, #nolint #add tiny weight to disaggregate properly
                             wdim = 1)
        
        ## aggregation of countries
      } else {
        zTr <- toolAggregate(z[setNames(list(c(tr$fromISO)), mainDimName), subTime, ],
                             bilatMapping[is.element(bilatMapping$toISO,
                                                     as.vector(outer(tr$toISO, getItems(z[setNames(list(c(tr$fromISO)), mainDimName), subTime, ], dim = secdDim), #nolint
                                                                     paste, sep = "."))),
                                          c("fromISO", "toISO")],
                             weight = NULL)
      }
     
      return(zTr)
    } 

       # loop over all transitions in dim 1
    dims <- getSets(m)[c(1,2)]
    mainDim <- 1.1
    
    for (a in tr) {
     mTr <-   .histDim(m, tr = a, mainDim = mainDim, dims = dims)
     # fill data
      if (overwrite == TRUE) {
        # only overwrite the exact combinations for which weighting existed, rest remains zero
        addM <- setdiff(getItems(mTr, dim = 1), getItems(m, dim = 1))
        if (length(addM) > 0) {
          addMissM <- new.magpie(cells_and_regions = addM,
                                 years = getItems(m, dim = 2),
                                 names = getItems(m, dim = 3),
                                 fill = 0)
          m <- mbind(m, addMissM)
        }

    m[getItems(mTr, dim = 1), getYears(mTr), ] <- mTr
      } else {
  m[getItems(mTr, dim =1), getYears(mTr), ][is.na(m[getItems(mTr, dim =1), getYears(mTr), ])] <- mTr[is.na(m[getItems(mTr, dim =1), getYears(mTr), ])] #nolint
      }

    } 

# do second dimension
mainDim <- 1.2 

  for (a in tr2) {
     mTr <-   .histDim(m, tr = a, mainDim = mainDim, dims = dims)
     # fill data
      if (overwrite == TRUE) {
        # only overwrite the exact combinations for which weighting existed, rest remains zero
        addM <- setdiff(getItems(mTr, dim = 1), getItems(m, dim = 1))
        if (length(addM) > 0) {
          addMissM <- new.magpie(cells_and_regions = addM,
                                 years = getItems(m, dim = 2),
                                 names = getItems(m, dim = 3),
                                 fill = 0)
          m <- mbind(m, addMissM)
        }

    m[getItems(mTr, dim = 1), getYears(mTr), ] <- mTr
      } else {
  m[getItems(mTr, dim =1), getYears(mTr), ][is.na(m[getItems(mTr, dim =1), getYears(mTr), ])] <- mTr[is.na(m[getItems(mTr, dim =1), getYears(mTr), ])] #nolint
      }
    } 
   
   
   
m[list("Partner" = "XSD"),,]  %>% dimSums(dim = c(1,3))
m[list("Partner" = c("SSD", "SDN")),,]  %>% dimSums(dim = c(1,3))

m[list("Partner" = "YUG"),,]  %>% dimSums(dim = c(1,3))
m[list("Partner" = c("SCG", "SVN", "HRV", "MKD", "BIH")),,]  %>% dimSums(dim = c(1,3))


   
    # do the same thing but for the dim 1.2
    for (a in tr2) {
      # check if new regions of transition exists in m
      toISOmissing   <- !all(is.element(a$toISO, getItems(m, dim = 1.2)))
      if (toISOmissing) {
        
        missingNew <- new.magpie(cells_and_regions =  as.vector(outer(getItems(m, dim = 1.1),
                                                                      a$toISO[which(!is.element(a$toISO, getItems(m, dim = 1.2)))], paste, sep = ".")), #nolint
                                 years = getItems(m, dim = 2),
                                 names = getItems(m, dim = 3),
                                 fill = 0)
        m <- mbind(m, missingNew)
        
        vcat(1, "Data was newly created and given equal weight for the following new countrys in dim 1.2: ",
             getItems(missingNew, dim = 1.1))
      }
      # create transformation matrix
      ## time span where the data need to be adjusted
      subTime <- getYears(m[, seq_len(which(getYears(m) == a$fromY)), ])
      # disaggregation of countries
      
      if (length(a$fromISO) == 1) {
        
        if (is.null(additional_weight)) {
          weight <- setYears(m[list("dim12" = c(a$toISO)), a$toY, ], NULL)
          
          # mapping needs to be subset to the exact combinations present in the weight for toolAggregate
          # if weight does not contain some of the combinations in m add it
          addR <- setdiff(getItems(m[list("dim12" = a$fromISO), subTime, ], dim = 1.1),
                          getItems(weight, dim = 1.1))
          
          if (length(addR) > 0) {
            addMiss <- new.magpie(cells_and_regions = as.vector(outer(addR, getItems(weight, dim = 1.2), paste, sep = ".")), #nolint
                                  years = getItems(weight, dim = 2),
                                  names = getItems(weight, dim = 3),
                                  fill = 0)
            weight <- mbind(weight, addMiss)
          }
          
          
          if (toISOmissing) {
            weight[] <- 1
          } # give equal weight here
          
          
          if (anyNA(weight)) {
            
            weight[is.na(weight)] <- 0
            vcat(0, "Weight 2 in toolISOhistorical contained NAs. Set NAs to 0!")
          }
        } else {
          
          stop("Additional weight not yet implemented for bilateral data ")
        }
        
        bilatMapping <- mapping # mapping needs to be made bilateral
        bilatMapping <- data.frame(toISO = as.vector(outer(getItems(m, dim = 1.1), bilatMapping$toISO, paste, sep = ".")),
                                   fromISO = as.vector(outer(getItems(m, dim = 1.1), bilatMapping$fromISO,  paste, sep = "."))) #nolint
        
        # mapping needs to be subset to the exact combinations present in the weight for toolAggregate
        
        mTr <- toolAggregate(m[list("dim12" = a$fromISO), subTime, ],
                             bilatMapping[is.element(bilatMapping$toISO,
                                                     getItems(weight[list("dim11" = getItems(m[list("dim12" = a$fromISO), , ], dim = 1.1)), , ], #nolint
                                                              dim = 1)),
                                          c("fromISO", "toISO")],
                             from = "fromISO", to = "toISO",
                             dim = 1,
                             weight = weight[list("dim11" = getItems(m[list("dim12" = a$fromISO), , ], dim = 1.1)), , ],
                             wdim = 1,
                             negative_weight = "allow")
        
        
        ## aggregation of countries
      } else {
        mTr <- toolAggregate(m[list("dim12" = a$fromISO), subTime, ],
                             bilatMapping[is.element(bilatMapping$toISO,
                                                     as.vector(outer(a$toISO, getItems(m[list("dim12" = a$fromISO), subTime, ], dim = 1.1), #nolint
                                                                     paste, sep = "."))),
                                          c("fromISO", "toISO")],
                             weight = NULL)
      }
      # fill data
      if (overwrite == TRUE) {
        addM <- setdiff(getItems(mTr, dim = 1), getItems(m, dim = 1))
        if (length(addM) > 0) {
          addMissM <- new.magpie(cells_and_regions = addM,
                                 years = getItems(m, dim = 2),
                                 names = getItems(m, dim = 3),
                                 fill = 0)
          m <- mbind(m, addMissM)
        }
        # only overwrite the exact combinations for which weighting existed, rest remains zero
        m[getItems(mTr, dim = 1), subTime, ] <- mTr
      } else {
        m[list("dim12" = a$toISO), subTime, ][is.na(m[list("dim12" = a$toISO), subTime, ])] <- mTr[is.na(m[list("dim12" = a$toISO), subTime, ])] #nolint
      }
    } # a in tr2 - transitions
    
    
    # delete old lines
    for (b in unique(mapping$fromISO)) {
      if (is.element(b, getItems(m, dim = 1.1))) {
        m <- m[list("dim11" = b), , inv = TRUE]
      }
      if (is.element(b, getItems(m, dim = 1.2))) {
        m <- m[list("dim12" = b), , inv = TRUE]
      }
    }
    
    # rename dims back to original ones
    getSets(m)[1] <- origDim1
    getSets(m)[2] <- origDim2
    
    
  } else {
    stop("not implemented for objects greater than 2 sub-dimensions in 1st dim")
  }
  
  
  
  return(m)
}
