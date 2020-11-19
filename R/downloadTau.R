#' @importFrom utils download.file unzip
downloadTau <- function(subtype="paper") {
  
  # Meta data provided by the user
  meta <- list()
  
  meta$url <- "http://www.pik-potsdam.de/members/dietrich/"
  #meta$version <-
  #meta$citation <-
  #meta$quality <- quality()
  
  file <- toolSubtypeSelect(subtype,c(paper="tau-paper.zip",historical="tau-historical.zip"))
  download.file(paste0(meta$url,file), destfile = "tau.zip")
  unzip("tau.zip")
  unlink("tau.zip")
  return(meta)
}