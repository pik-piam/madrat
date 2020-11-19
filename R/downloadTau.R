#' @importFrom utils download.file unzip
downloadTau <- function(subtype="paper") {
  
  # Meta data to be provided by the user
  meta <- list(url      = "http://www.pik-potsdam.de/members/dietrich/",
               title    = "Agricultural Land Use Intensity Tau",
               authors  = "Jan Philipp Dietrich",
               subtype  = "Creative Commons Attribution-ShareAlike 4.0 International License"
               #version <- 
               #date_released <- 
               #doi <- 
  )
  
  file <- toolSubtypeSelect(subtype,c(paper="tau-paper.zip",historical="tau-historical.zip"))
  download.file(paste0(meta$url,file), destfile = "tau.zip")
  unzip("tau.zip")
  unlink("tau.zip")
  return(meta)
}