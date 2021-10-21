#' commandAvailable
#'
#' Check if a command line tool is available by trying to run `which <command>`.
#'
#' @param command The command to check, e.g. ls.
#' @return TRUE or FALSE, depending on whether the command is available.
commandAvailable <- function(command) {
  return(tryCatch({
    identical(system2("which", command, stdout = NULL, stderr = NULL), 0L)
  }, warning = function(w) FALSE, error = function(e) FALSE))
}
