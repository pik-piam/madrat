#' commandAvailable
#'
#' Check if a command line tool is available by trying to execute it without any arguments.
#'
#' @param command The command to check, e.g. ls.
#' @return TRUE or FALSE, depending on whether the command is available.
#' @importFrom withr local_dir local_tempdir
commandAvailable <- function(command) {
  local_dir(local_tempdir())
  return(tryCatch({
    system2(command, stdout = NULL, stderr = NULL)
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE))
}
