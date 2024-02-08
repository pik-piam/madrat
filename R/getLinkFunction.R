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

  dir.create("aa")
  writeLines("c", "aa/c")

  if (suppressWarnings(file.symlink("aa", "bb")) && readLines("bb/c") == "c") {
    dirLink <- file.symlink
  } else if (suppressWarnings(Sys.junction("aa", "bb")) && readLines("bb/c") == "c") {
    dirLink <- Sys.junction
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
