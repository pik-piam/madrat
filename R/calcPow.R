#' @importFrom compiler cmpfun

calcPow <- function(input=array(2, c(10,5,1))){
  
  # create a variable which has to be exported to the workers
   input <- input
   pow <- function(exponent){
   # as magclass is not part of base, therefore the library(magclass has to be evaluated on all the workers)
   return(as.magpie(input^exponent))
   }
   
  #actuall madlapply call 
   resultnopar <- madlapply(X=c(2:10), FUN=pow, #stating X and FUN
                            exports=list(list(c("input"),expression(environment()))), # listing the objects or function to be exported and their origin environments
                            evals=c("magclass")) # libraries to be evaluated 

    x=mbind(resultnopar)# combine all listentries in one magpieobject
    return(list(x=x,weight=NULL, min=min(x),
                max=max(x),
                unit="-",
                description="Some random example",
                note=c("Some note")))
   
   
   
  }