#' getMainfolder
#' 
#' Functions checks for a global setting of the mainfolder (either by setting the environment
#' variable "MADRAT_MAINFOLDER" or by setting the R option with the same name). If none of these
#' is available the user will be asked for a directory. If this is not provided a temporary folder
#' will be used. 
#' 
#' @param verbose boolean deciding whether status information/updates should be shown or not
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{initializeConfig}}, \code{\link{getConfig}}, \code{\link{setConfig}}
#' 
getMainfolder <- function(verbose=TRUE) {
  
  # option MADRAT_MAINFOLDER set?
  mainfolder <- getOption("MADRAT_MAINFOLDER")
  if(!is.null(mainfolder)) return(mainfolder)
  
  # environment variable MADRAT_MAINFOLDER set?
  mainfolder <- Sys.getenv("MADRAT_MAINFOLDER", unset = NA)
  if(!is.na(mainfolder)) return(mainfolder)
  
  # ask for folder to use
  if(interactive()){
    repeat {
      s <- tolower(readline("madrat mainfolder for data storage not set! Do you want to set it now? (y/n) "))
      if(s %in% c("y","n")) break
    }
    if(s=="y") {
      repeat {
        folder <- gsub('"',"",readline("Please enter main folder path: "), fixed = TRUE)
        if(!dir.exists(folder)) {
          s <- tolower(readline("Directory does not exist. Should it be created? (y/n) "))
          if(s=="y") dir.create(folder, recursive = TRUE)
        }
        if(dir.exists(folder)) {
          mainfolder <- normalizePath(folder, winslash = "/")
          repeat {
            s <- tolower(readline("Should this path be added to your global .Rprofile to be used permanently? (y/n) "))
            if(s %in% c("y","n")) break
          }
          if(s=="y") write(c("","# Set mainfolder for madrat package",
                             paste0('options(MADRAT_MAINFOLDER="',mainfolder,'")'),"")
                           , file="~/.Rprofile", append=TRUE)
          return(mainfolder)
        } else {
          message("Please specify either an existing folder or a folder you would like to create!")
        }
      }
    } 
  } 
  
  # use temporary directory
  if(verbose) message("Temporary main folder will be used..")
  mainfolder <- paste0(tempdir(),"/madrat")
  if(!dir.exists(mainfolder)) dir.create(mainfolder)
  return(mainfolder)
}