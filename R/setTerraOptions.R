#' setTerraOptions
#'
#' Uses `terra::terraOptions` to temporarily (until the current scope ends) set the terra
#' tempdir to a temporary folder in `getConfig("tmpfolder")`.
#' This way temp files are written ot the tempfolder specified by the madrat config. Also
#' `todisk` is set to TRUE and `memfrac` to 0.5, these are based on experience and reduce memory problems.
#' @return Invisibly, the list of terra options
#' @author Pascal Führlich
#' @export
setTerraOptions <- function() {
  terraTemp <- withr::local_tempdir(tmpdir = getConfig("tmpfolder"), .local_envir = parent.frame())

  originalTerraOptions <- terra::terraOptions(print = FALSE)
  withr::defer_parent({
    terra::terraOptions(tempdir = originalTerraOptions$tempdir,
                        todisk = originalTerraOptions$todisk,
                        memfrac = originalTerraOptions$memfrac)
  })
  terra::terraOptions(tempdir = terraTemp,
                      todisk = TRUE,
                      memfrac = 0.5)

  return(invisible(terra::terraOptions(print = FALSE)))
}
