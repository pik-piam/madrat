#' getLinkFunction
#'
#' Returns a function that creates a symlink, hardlink, junction, or copy of
#' files and directories, depending on OS capabilities (usually symlinks are
#' not supported on Windows).
#'
#' @return A function with arguments "from" and "to" which should behave like
#' file.symlink on all platforms.
#' @author Pascal Sauer
#' @export
getLinkFunction <- function() {
  withr::local_dir(withr::local_tempdir())

  writeLines("a", "a")
  if (suppressWarnings(file.symlink("a", "b")) && readLines("b") == "a") {
    fileLink <- file.symlink
  } else if (suppressWarnings(file.link("a", "b")) && readLines("b") == "a") {
    fileLink <- file.link
  } else {
    message("neither file.symlink nor file.link work for files, falling back to copying")
    fileLink <- file.copy
  }

  # Sys.junction only exists on Windows
  createJunction <- get0("Sys.junction", ifnotfound = function(...) FALSE)

  dir.create("aa")
  writeLines("c", "aa/c")
  if (suppressWarnings(file.symlink("aa", "bb")) && readLines("bb/c") == "c") {
    dirLink <- file.symlink
  } else if (suppressWarnings(createJunction("aa", "bb")) && readLines("bb/c") == "c") {
    dirLink <- createJunction
  } else {
    message("neither file.symlink nor Sys.junction work for directories, falling back to copying")
    dirLink <- function(...) file.copy(..., recursive = TRUE)
  }

  link <- function(from, to) {
    return(Map(from, to, f = function(fromPath, toPath) {
      if (dir.exists(fromPath)) {
        return(dirLink(fromPath, toPath))
      } else {
        return(fileLink(fromPath, toPath))
      }
    }))
  }

  return(link)
}
