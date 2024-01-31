#' redirectSource
#'
#' Redirect a given dataset type to a different source folder. The redirection
#' is local, so it will be reset when the current function call returns. See
#' example for more details.
#'
#' @param type Dataset name, e.g. "Tau" for \code{\link{readTau}}
#' @param target Either path to the new source folder that should be used instead of the default,
#' or a list/vector of paths to files which are then symlinked into a temporary folder that is then
#' used as target folder, or NULL to remove the redirection
#' @param ... Additional arguments, passed on to source-specific inject function if it exists
#' @param .local The scope of the redirection, passed on to setConfig. Defaults to the current function.
#' Set to an environment for more control or to FALSE for a permanent/global redirection.
#' @return Invisibly, the source folder that is now used for the given type
#' @author Pascal Sauer
#' @examples \dontrun{
#' f <- function() {
#'   redirectSource("Tau", target = "~/TauExperiment")
#'   # the following call will change directory
#'   # into ~/TauExperiment instead of <getConfig("sourcefolder")>/Tau
#'   readSource("Tau")
#' }
#' f()
#' # Tau is only redirected in the local environment of f,
#' # so it will use the usual source folder here
#' readSource("Tau")
#' }
#' @export
redirectSource <- function(type, target, ..., .local = TRUE) {
  # Redirecting only specific subtypes is not supported to avoid tricky cases
  # where the subtype is ignored (search for "getSourceFolder\(.*subtype = NULL\)").

  # TODO call source-specific redirect function if it exists

  if (is.environment(.local)) {
    .localEnvir <- .local
  } else if (.local) {
    .localEnvir <- parent.frame()
  } else {
    .localEnvir <- globalenv()
  }

  if (!is.null(target)) {
    preservedNames <- names(target)
    target <- normalizePath(target, mustWork = TRUE)
    names(target) <- preservedNames
    if (length(target) >= 2 || !dir.exists(target)) {
      # redirect to files
      tempDir <- withr::local_tempdir(.local_envir = .localEnvir)
      if (is.null(names(target))) {
        names(target) <- basename(target)
      } else {
        # append basename to target path if it ends with "/"
        i <- endsWith(names(target), "/")
        names(target)[i] <- paste0(names(target)[i], basename(target[i]))

        for (p in file.path(tempDir, names(target))) {
          if (!dir.exists(dirname(p))) {
            dir.create(dirname(p), recursive = TRUE)
          }
        }
      }
      file.symlink(target, file.path(tempDir, names(target)))

      # symlink all other files in original source folder
      # TODO test thoroughly
      parentFolders <- function(path, collected = NULL) {
        if (path == ".") {
          return(collected)
        }
        return(parentFolders(dirname(path), c(path, collected)))
      }

      dontlink <- lapply(names(target), parentFolders) # find all parent folders
      dontlink <- unique(do.call(c, dontlink)) # flatten and remove duplicates

      sourceFolder <- getSourceFolder(type, subtype = NULL)
      withr::with_dir(sourceFolder, {
        dirs <- Filter(dir.exists, dontlink)
        linkThese <- lapply(c(".", dirs), dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      })
      linkThese <- do.call(c, linkThese)
      linkThese <- sub("^\\./", "", linkThese)
      linkThese <- setdiff(linkThese, dontlink)
      if (length(linkThese) > 0) {
        file.symlink(file.path(sourceFolder, linkThese),
                     file.path(tempDir, linkThese))
      }

      target <- tempDir
    }
    # paths inside the source folder use the fileHashCache system, see getHashCacheName,
    # to prevent that we need to make sure that the target is not inside the source folder
    stopifnot(!startsWith(normalizePath(target), normalizePath(getConfig("sourcefolder"))))
  }

  redirections <- getConfig("redirections")
  redirections[[type]] <- target
  setConfig(redirections = redirections, .local = .localEnvir)
  return(invisible(target))
}
