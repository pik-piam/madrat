#' @importFrom utils download.file unzip
downloadTau <- function() {
  download.file("http://www.pik-potsdam.de/members/dietrich/tau-data.zip",destfile = "tau-data.zip")
  unzip("tau-data.zip")
  unlink("tau-data.zip")  
}