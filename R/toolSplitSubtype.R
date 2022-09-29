#' Tool: SplitSubtype
#' 
#' This function can split a subtype string into smaller entities based on 
#' a given separator and check whether these entities exist in a reference list
#' 
#' @param subtype A character string which can be split with the given separator into smaller entities
#' @param components A named list with the same length as the subtype has entities. Names of the list
#' are used as names of the entities while the content of each list element represents the allowed values
#' of that given entity. If all values are allowed use NULL as entry.
#' @param sep separator to be used for splitting
#' @return A named list with the different entities of the given subtype
#' @author Jan Philipp Dietrich
#' @examples
#' toolSplitSubtype("mymodel:myversion:myworld", list(model=c("mymodel","notmymodel"), 
#'                                                    version=c("myversion","42"), 
#'                                                    world="myworld"))
#' 
#' @export
toolSplitSubtype <- function(subtype, components, sep = ":") {
  out <- strsplit(subtype, sep)[[1]]
  
  if (length(out) != length(components)) stop("Subtype does not follow expected structure (should contain ", 
                                              length(components), " elements separated by \"",sep,"\")!")
  names(out) <- names(components)
  .validCheck <- function(i,choice,avail) {
    if (is.null(avail[[i]])) return(TRUE)
    return(choice[i] %in% avail[[i]])
  }
  valid <- sapply(names(out), .validCheck, out, components)
  if (!all(valid)) stop("Invalid subtype selection: \"", paste(out[!valid],collapse = "\", \""),"\"")
  return(as.list(out))
}
