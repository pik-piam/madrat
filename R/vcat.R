#' Tool: Verbosity Cat
#' 
#' Function which returns information based on the verbosity setting
#' 
#' 
#' @param verbosity The lowest verbosity level for which this message should be
#' shown (verbosity = -1 means no information at all, 0 = only warnings, 1 =
#' warnings and execution informations, 2 = full information). If the verbosity
#' is set to 0 the message is written as warning, if the verbosity is set
#' higher than 0 it is written as a normal cat message.
#' @param ... The message to be shown
#' @param level This argument allows to establish a hierarchy of print
#' statements. The hierarchy is preserved for the next vcat executions.
#' Currently this setting can have 4 states: NULL (nothing will be changed), 0
#' (reset hierarchies), "+" (increase hierarchy level by 1) and "-" (decrease
#' hierarchy level by 1).
#' @param fill a logical or (positive) numeric controlling how the output is 
#' broken into successive lines. If FALSE (default), only newlines created 
#' explicitly by "\\n" are printed. Otherwise, the output is broken into lines 
#' with print width equal to the option width if fill is TRUE, or the value of 
#' fill if this is numeric. Non-positive fill values are ignored, with a warning.
#' @param show_prefix a logical defining whether a content specific prefix (e.g. "NOTE")
#' should be shown in front of the message or not. If prefix is not shown it will also
#' not show up in official statistics.
#' @export
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' vcat(2,"Hello world!")
#' }
#' @importFrom utils capture.output
vcat <- function(verbosity,...,level=NULL, fill=TRUE, show_prefix=TRUE) {
  #write output based on set verbosity level
  
  if (!is.null(level)) {
    if (level == 0) {
      options(gdt_nestinglevel = NULL)
    } else if (level == "-") {
      # remove empty space
      options(gdt_nestinglevel = substring(getOption("gdt_nestinglevel"),2))
      if (getOption("gdt_nestinglevel") == "") options(gdt_nestinglevel = NULL)
    }
  }  
  
  d <- getConfig("diagnostics")
  if (is.character(d)) {
    writelog <- TRUE
    logfile <-  paste0(getConfig("outputfolder"),"/",d,".log")
    fulllogfile <- paste0(getConfig("outputfolder"),"/",d,"_full.log")
  } else {
    writelog <- FALSE
  }
  prefix <- c("", "ERROR: ", "WARNING: ", "NOTE: ", "MINOR NOTE: ")[min(verbosity,2) + 3]
  if (prefix == "" | !show_prefix) prefix <- NULL
  if (writelog && dir.exists(dirname(fulllogfile))) {
    base::cat(c(prefix,...), fill = fill, sep = "",labels = getOption("gdt_nestinglevel"), file = fulllogfile, append = TRUE) 
  }
  if (getConfig("verbosity") >= verbosity) {
    if (writelog && dir.exists(dirname(logfile))) {
      base::cat(c(prefix,...), fill = fill, sep = "", labels = getOption("gdt_nestinglevel"), file = logfile, append = TRUE)
    }
    if (verbosity == -1) {
      base::stop(..., call. = FALSE)      
    } else if (verbosity == 0) {
      base::warning(..., call. = FALSE) 
      message(paste(capture.output(base::cat(c(prefix,...), fill = fill, sep = "",
                                             labels = getOption("gdt_nestinglevel"))), collapse = "\n"))
    } else {
      message(paste(capture.output(base::cat(c(prefix,...), fill = fill, sep = "",
                                             labels = getOption("gdt_nestinglevel"))), collapse = "\n"))  
    }
  }
  
  if (!is.null(level)) {
    if (level == "+") {
      # add empty space
      options(gdt_nestinglevel = paste0(">", getOption("gdt_nestinglevel")))
    }
  }
  
}

#create an own warning function which redirects calls to vcat (package internal)
warning <- function(...) {
  vcat(0,...)
}

# create a own stop function which redirects calls to stop (package internal)
stop <- function(...) {
  vcat(-1,...)
}


# create an own cat function which redirects calls to cat (package internal)
cat <- function(...) {
  vcat(1,...)
}

