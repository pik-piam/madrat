#' @importFrom utils download.file unzip
downloadTau <- function(subtype="paper") {
  file <- toolSubtypeSelect(subtype,c(paper="tau-paper.zip",historical="tau-historical.zip"))
  download.file(paste0("http://www.pik-potsdam.de/members/dietrich/",file), destfile = "tau.zip")
  unzip("tau.zip")
  unlink("tau.zip")  
}