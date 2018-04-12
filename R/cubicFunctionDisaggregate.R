#' cubicFunctionDisaggregate
#' 
#' Estimates cubic function inverses based on a weight factor that sum up to the 
#' original cubic function (sum in the x-axis)
#' 
#' Use case: disaggregate a single region cubic cost function to multiple country
#' cubic functions weighted by a contribution factor. The sum of the countries 
#' function output is equal to the original regional function. 
#' 
#' input: coefficients of the n-th country level cubic cost function. 
#' 
#' Description of the problem: the disaggregation of functions that represent unit
#' costs (or prices) in the y-axis and quantities in the x-axis require operations
#' with the inverse of the original functions.  As complex functions present 
#' analytically challenging inverse function derivations, we adopt a sampling
#' method to derive the function that corresponds to the sum of cubic function 
#' inverses.     
#' 
#' Further extensions: the R function can be extended to support more complex curve
#' estimations (beyond third degree), whenever the mathematical function have a well
#' defined inverse function in the selected boundaries.
#' 
#' @param data magclass object that should be aggregated or data frame with
#' coefficients as columns. 
#' @param weight magclass object containing weights which should be considered
#' for a weighted aggregation. The provided weight should only contain positive
#' values, but does not need to be normalized (any positive number>=0 is allowed).
#' @param rel relation matrix containing a region mapping.
#' A mapping object should contain 2 columns in which each element of x
#' is mapped to the category it should belong to after (dis-)aggregation
#' @param xLowerBound numeric. Lower bound for x sampling (default=0). 
#' @param xUpperBound numeric. Upper bound for x sampling (default=100).
#' @param returnMagpie boolean. if true, the function will return a single data table
#' with all the countries in MagPie format. returnChart and returnSample are set to
#' FALSE automatically if this option is active (default=TRUE).
#' @param returnCoeff boolean. Return estimated coefficients (default=TRUE).
#' @param returnChart boolean. Return chart (default=FALSE).
#' @param returnSample boolean. Return samples used on estimation (default=FALSE).
#' @param numberOfSamples numeric. NUmber of y-axis samples used on estimation 
#' (default=1e3).
#' @param unirootLowerBound numeric. Lower bound to search for inverse solution in the
#' initial bounds (default = -10).
#' @param unirootUpperBound numeric. Upper bound to search for inverse solution in the
#' initial bounds (default = 1e100).  
#' @param colourPallete vector. colour pallete to use on chart (default=FALSE).
#' @param label list. List of chart labels (default=list(x = "x", y = "y", legend =
#' "legend")).
#' 
#' @return return: returns a list of magpie objects containing the coefficients for the
#' aggregate function. If returnMagpie is FALSE, returns a list containing the 
#' coefficients for the aggregate function (returnCoeff=TRUE), charts (returnChart=FALSE)
#' and/or samples used in the estimation (returnSample=FALSE).
#' 
#' @author Renato Rodrigues
#' @export
#' @importFrom magclass is.magpie as.data.frame
#' @importFrom reshape2 acast
#' @importFrom stats reshape uniroot lm
#' @seealso \code{\link{cubicFunctionAggregate}}
#' @examples
#' 
#' # Example
#' # LAM coefficients
#' df <- setNames(data.frame(30,50,0.34369,2),c("c1","c2","c3","c4"))
#' row.names(df) <- "LAM"
#' # weight
#' weight <- setNames(c(21,0,579,3,228),c("ARG","BOL","BRA","CHL","COL"))
#' # maxExtraction (upper limit for function estimation)
#' maxExtraction <- 100
#' # output
#' output <- cubicFunctionDisaggregate(df, weight,xUpperBound=maxExtraction,
#'  returnMagpie=FALSE,returnChart=TRUE,returnSample=TRUE,
#'  label=list(x="Cumulated Extraction", y="Cost", legend="Region Fuel Functions"))#' output$chart
#' output$coeff
#' output$chart


cubicFunctionDisaggregate <- function(data, weight, rel=NULL, xLowerBound=0, xUpperBound=100, returnMagpie=TRUE, returnCoeff=TRUE, returnChart=FALSE, returnSample=FALSE, numberOfSamples=1e3, unirootLowerBound = -10,unirootUpperBound = 1e100, colourPallete=FALSE, label = list(x = "x", y = "y", legend = "legend")){
  
  ### Start of cubicFitDisaggregate function
  
    cubicFitDisaggregate <- function(data, weight, xLowerBound=0, xUpperBound=100, returnCoeff=TRUE, returnChart=FALSE, returnSample=FALSE, numberOfSamples=1e3, unirootLowerBound = -10,unirootUpperBound = 1e100, colourPallete=FALSE, label = list(x = "x", y = "y", legend = "legend")){
      
      # initialize coefficients list
      coeffList <- lapply(names(weight),function(x){ 
        row <- rep(0, length(names(data)))
        names(row) <- names(data)
        return(row)
      }
      )
      names(coeffList) <- names(weight)
      
      if (length(weight[weight != 0]) == 1){ # no need to disaggregate a single function
        # preparing results
        result <- list()
        singleWeight <- names(weight[weight != 0])
        coeffList[[singleWeight]][] <- data
        if (returnChart == TRUE){
          thirdDegreeFunction <-  function(x) {
            return( as.numeric(coeffList[[singleWeight]][1]) + as.numeric(coeffList[[singleWeight]][2])*x + as.numeric(coeffList[[singleWeight]][3])*x^2 + as.numeric(coeffList[[singleWeight]][4])*x^3 )
          }
          p <- ggplot2::ggplot(data = NULL)
          p <- p + ggplot2::xlim(xLowerBound, xUpperBound)
          p <- p + ggplot2::stat_function(fun = thirdDegreeFunction, size=1, ggplot2::aes(colour = "_aggregated function", linetype = "_aggregated function"), na.rm=TRUE)
          p <- p + ggplot2::scale_linetype_manual(values = c("solid"), guide = FALSE)
          p <- p + ggplot2::labs(colour = label$legend, x = label$x, y = label$y)
          result$chart <- p # return chart
        }
        if (returnCoeff == TRUE){ # return coeff of estimated function
          if(length(result) == 0) {
            result <- coeffList 
          } else {
            result$coeff <- coeffList 
          }
        }
        return(result)  
      } 
      
      #function to be disaggregated
      fTotal <- function(x){ as.numeric(data[1]) + as.numeric(data[2])*x + as.numeric(data[3])*x^2 + as.numeric(data[4])*x^3 }  
      
      inverse = function (f, lower = unirootLowerBound, upper = unirootUpperBound) {
        function (y) { 
          result <- uniroot((function (x) f(x) - y), lower = lower, upper = upper, extendInt = "yes")$root 
          #tryCatch(
          #  result <- uniroot((function (x) f(x) - y), lower = lower, upper = upper, extendInt = "yes",maxiter = 10000, trace =2)$root, 
          #  error = return(NA)
          #)
          return(result)
        }
      }
      
      fTotalInverse <- function(x,lower = unirootLowerBound, upper = unirootUpperBound){
        lis<-vector()
        for(i in x){
          lis<-append(lis,inverse(fTotal,lower,upper)(i))
        }
        return(lis)
      } 
      
      #Boundaries for which all functions are defined
      #X (= sum X of each function)
      maxX <- xUpperBound
      minX <- xLowerBound
      #Y
      maxY <- fTotal(xUpperBound)
      minY <- fTotal(xLowerBound)
      minY <- max(c(0,minY)) # negative y do not make sense (avoid negative prices)

      # Sampling
      # sampling x
      samples <- data.frame(x = seq(from=minX, to=maxX, length.out = numberOfSamples))
      # sampling y
      samples$y <- fTotal(samples$x)
      
      # sampling y
      totalWeight <- sum(weight)
      for (rowName in names(weight)){
          samples[,(paste0(rowName,".x"))] <- samples$x*(weight[rowName]/totalWeight)
      }
      
      # estimating functions to each row from the new samples created from weights
      for (rowName in names(weight)){
        if (weight[rowName]!=0){
          current <- data.frame(x = samples[paste0(rowName,".x")], y = samples[,"y"])
          names(current) <- c("x","y")
          closestToZero <- sapply(current, function(x) x[which.min(abs(x))])
          tol <- 1e-4
          if ((closestToZero["x"] > -tol) & (closestToZero["x"] < tol)){ # if there is a x value close to zero, estimate the function with a fixed intercept (to approximate the intercept of the original functions) 
            intercept <- rep.int(closestToZero["y"], length(current$x))
            newFunction <- lm(current$y~-1+current$x+I(current$x^2)+I(current$x^3)+offset(intercept))
            newFunctionCoeff <- c(closestToZero["y"], as.vector(summary(newFunction)$coefficients[,1]))
          } else {
            newFunction <- lm(current$y ~ poly(current$x, 3, raw=TRUE))
            newFunctionCoeff <- as.vector(summary(newFunction)$coefficients[,1])
          }
          names(newFunctionCoeff) <- names(data)
          coeffList[[rowName]][] <- newFunctionCoeff
        }
      }
      
      # preparing results
      result <- list()
      if (returnSample == TRUE){
        result$sample <- samples # return samples table
      }
      if (returnChart == TRUE){
        
        #estimated functions
        fY <- lapply(coeffList, function(coef){ function(x){ as.numeric(coef[1]) + as.numeric(coef[2])*x + as.numeric(coef[3])*x^2 + as.numeric(coef[4])*x^3 } })
        
        p <- ggplot2::ggplot(samples, ggplot2::aes(samples$x, samples$y, group = 1)) +
          #ggplot2::geom_point(size=1) +
          ggplot2::ylim(0, max(samples$y)) +
          ggplot2::xlim(0, max(samples$x))
        p <- p + ggplot2::stat_function(fun=fTotal, size=1, ggplot2::aes(colour = "_aggregated function", linetype = "_aggregated function"), na.rm=TRUE)
        for (i in 1:(length(weight))){
          p <- p + eval(parse(text = paste0("ggplot2::stat_function(fun=fY[[\"", as.character(names(weight)[i]) , "\"]], ggplot2::aes(colour = \"", as.character(names(weight)[i]) , "\" , linetype = \"" , as.character(names(weight)[i]), "\"), na.rm=TRUE)"))) #hack to allow legend
        }
        if ( !(colourPallete[1] == FALSE) & (length(colourPallete) >= length(weight))){
          p <- p + ggplot2::scale_colour_manual(label$legend, values = colourPallete)
        }
        p <- p + ggplot2::scale_linetype_manual(values = c("solid", rep.int("dashed", length(weight))), guide = FALSE)
        
        p <- p + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linetype = c("solid", rep.int("dashed", length(weight))))))
        
        p <- p + ggplot2::labs(colour = label$legend, x = label$x, y = label$y)
        
        result$chart <- p # return chart
        
      }
      if (returnCoeff == TRUE){ # return coeff of estimated function
        if(length(result) == 0) {
          result <- coeffList 
        } else {
          result$coeff <- coeffList 
        }
      }
      return(result)
    }
    
  ### End of cubicFitDisaggregate function

  # pre processing data formats and executing estimations
  if(is.magpie(data)){
    df <- as.data.frame(data)
    # splitting large dimensional magpie objects
    dataNames <- names(df[,grep("Data", names(df))]) # all data names
    dataNames <- dataNames[-length(dataNames)] # remove last element (coefficient labels)
    factorGroups <- interaction(df[,dataNames]) # all combinations of Data values
    groupsList <- split(df, with(df, factorGroups), drop = TRUE)
    #looping through all data sets and estimating the respective aggregated functions 
    output <- lapply(seq_along(groupsList),
                     function(i) {
                       # preparing data (row names equal to regions, one column for each coefficient)
                       currentDf <- groupsList[[i]]
                       currentDf <- currentDf[c(2,length(currentDf)-1,length(currentDf))] #region, coeff, value 
                       names(currentDf) <- c("Region","coeff","value")  
                       currentDf <- reshape2::acast(currentDf, Region ~ coeff, value.var = 'value')
                       currentWeight <- as.data.frame(weight[[names(groupsList[i])]])[c("Value")]
                       rownames(currentWeight) <- getRegions(weight[[names(groupsList[i])]])
                       # estimating aggregated function
                       if (is.null(rel)){ # single aggregated function
                         out <- cubicFitDisaggregate(currentDf, currentWeight, xLowerBound=xLowerBound, xUpperBound=xUpperBound, returnCoeff=returnCoeff, returnChart=returnChart, returnSample=returnSample, numberOfSamples=numberOfSamples, unirootLowerBound=unirootLowerBound,unirootUpperBound=unirootUpperBound, colourPallete=colourPallete, label=label)
                       } else { # looping through new regions and estimating the aggregated function
                         if (returnMagpie==TRUE){
                           returnCoeff=TRUE
                           returnChart=FALSE
                           returnSample=FALSE
                         }
                         from <- ifelse(dim(rel)[2]==3,2,1) # country
                         to <- ifelse(dim(rel)[2]==3,3,2) # region
                         out <- sapply(unique(rel[[to]]), function(region) {
                           currentFilteredDf <- currentDf[region,]
                           currentWeight <- currentWeight[rel[from][rel[to]==as.character(region)],]
                           names(currentWeight) <- rel[from][rel[to]==as.character(region)]
                           outRegion <- cubicFitDisaggregate(currentFilteredDf, currentWeight, xLowerBound=xLowerBound, xUpperBound=as.numeric(xUpperBound[region,,names(groupsList[i])]), returnCoeff=returnCoeff, returnChart=returnChart, returnSample=returnSample, numberOfSamples=numberOfSamples, unirootLowerBound=unirootLowerBound,unirootUpperBound=unirootUpperBound, colourPallete=colourPallete, label=label)
                           return(outRegion)
                         }) 
                         names(out) <- unique(rel[[to]])
                         if (returnMagpie==TRUE){
                           df <- out
                           df <- data.frame(sapply(unique(names(df)), function(name) df[[name]] )) # unlist results
                           out <- data.frame(t(df[]))
                           names(out) <- rownames(df)
                           rownames(out) <- gsub(".*\\.", "", names(df))
                           out <- stats::reshape(out, direction='long', varying=names(out), v.names='Value', timevar='coeff',times=names(out), idvar='Region', ids = rownames(out)) # long format
                           out <- as.magpie(out[,c("Region","coeff","Value")],temporal=0,datacol=3)
                         }
                       }
                       return(out)
                     })
    names(output) <- names(groupsList)
  } else {
    if (is.null(rel)){ # single aggregated function
      output <- cubicFitDisaggregate(data, weight, xLowerBound=xLowerBound, xUpperBound=xUpperBound, returnCoeff=returnCoeff, returnChart=returnChart, returnSample=returnSample, numberOfSamples=numberOfSamples, unirootLowerBound=unirootLowerBound,unirootUpperBound=unirootUpperBound, colourPallete=colourPallete, label=label)
    } else { # looping through new regions and estimating the aggregated function
      if (returnMagpie==TRUE){
        returnCoeff=TRUE
        returnChart=FALSE
        returnSample=FALSE
      }
      from <- ifelse(dim(rel)[2]==3,2,1) # country
      to <- ifelse(dim(rel)[2]==3,3,2) # region
      output <- sapply(unique(rel[[to]]), function(region) {
        currentFilteredDf <- data[region,]
        currentWeight <- weight[rel[from][rel[to]==as.character(region)],]
        outRegion <- cubicFitDisaggregate(currentFilteredDf, currentWeight, xLowerBound=xLowerBound, xUpperBound=xUpperBound, returnCoeff=returnCoeff, returnChart=returnChart, returnSample=returnSample, numberOfSamples=numberOfSamples, unirootLowerBound=unirootLowerBound,unirootUpperBound=unirootUpperBound, colourPallete=colourPallete, label=label)
        return(outRegion)
      })
      names(output) <- unique(rel[[to]])
      if (returnMagpie==TRUE){
        df <- output
        df <- data.frame(sapply(unique(names(df)), function(name) df[[name]] )) # unlist results
        output <- data.frame(t(df[]))
        names(output) <- rownames(df)
        rownames(output) <- gsub(".*\\.", "", names(df))
        output <- stats::reshape(output, direction='long', varying=names(output), v.names='Value', timevar='coeff',times=names(output), idvar='Region', ids = rownames(output)) # long format
        output <- as.magpie(output[,c("Region","coeff","Value")],temporal=0,datacol=3)
      }
    }
  } 
  return(output)
}







