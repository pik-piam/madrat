#' Compare a madrat function's output with and without your changes
#'
#' With `compareMadratOutputs` you can easily compare the output of a madrat function (read, calc, ...) with and without
#' your changes. First, run `compareMadratOutputs` without your changes, so a `.rds` file with the original output is
#' saved. Then apply your changes and run `compareMadratOutputs` again to compare the new output to the original output.
#'
#' For comparing `waldo::compare` is used if available, otherwise `all.equal`. If there are differences a `*-new.rds`
#' containing the new output is saved for closer inspection. All files are created in the current working directory.
#'
#' @param package [character(1)] The package where the given function is located.
#' @param functionName The name of the function from which you want to compare outputs.
#' @param subtypes The subtypes you want to check. For calc functions this must be NULL.
#'
#' @examples
#' \dontrun{
#' # save readFRA2020-old.rds
#' compareMadratOutputs("mrcommons", "readFRA2020", c("forest_area", "deforestation")
#'
#' # now apply your changes to mrcommons:::readFRA2020, reinstall mrcommons, restart the R session
#'
#' # compare to readFRA2020-old.rds
#' compareMadratOutputs("mrcommons", "readFRA2020", c("forest_area", "deforestation")
#' }
#'
#' @author Pascal Führlich
#'
#' @importFrom utils askYesNo
#' @importFrom withr with_package
#' @export
compareMadratOutputs <- function(package, functionName, subtypes) {
  oldRds <- paste0(functionName, "-old.rds")
  if (!file.exists(oldRds) && !askYesNo(paste0("Are you using the original/pre-refactoring/unchanged ",
                                               functionName, " right now?"))) {
    stop("You need to run compareMadratOutputs with the original/pre-refactoring/unchanged ", functionName,
         " first, so you can compare the output of your changed version with the original output.")
  }

  setConfig(ignorecache = functionName, .local = TRUE)
  if (is.null(subtypes)) {
    subtypes <- list(NULL)
  }
  output <- lapply(subtypes, function(subtype) {
    local_package(package)
    if (startsWith(functionName, "download")) {
      return(downloadSource(sub("^download", "", functionName), subtype = subtype))
    } else if (startsWith(functionName, "read")) {
      return(readSource(sub("^read", "", functionName), subtype = subtype, convert = FALSE))
    } else if (startsWith(functionName, "correct")) {
      return(readSource(sub("^correct", "", functionName), subtype = subtype, convert = "onlycorrect"))
    } else if (startsWith(functionName, "convert")) {
      return(readSource(sub("^convert", "", functionName), subtype = subtype, convert = TRUE))
    } else if (startsWith(functionName, "calc")) {
      stopifnot(is.null(subtype))
      return(calcOutput(sub("^calc", "", functionName), aggregate = FALSE))
    }
  })

  if (file.exists(oldRds)) {
    oldOutput <- readRDS(oldRds)
    if (identical(oldOutput, output)) {
      message("no differences")
    } else {
      saveRDS(output, paste0(functionName, "-new.rds"))
      message("Saved '", functionName, "-new.rds'. Found some differences:")
      if (requireNamespace("waldo", quietly = TRUE)) {
        print(waldo::compare(oldOutput, output))
      } else {
        message("To get nicer differences output you can run\n",
                "install.packages('waldo')")
        print(all.equal(oldOutput, output))
      }
    }
  } else {
    saveRDS(output, oldRds)
    message("Saved '", oldRds, "'. Now apply your changes to ", functionName,
            " and re-run compareMadratOutputs to compare the output of your changed version to the original output.")
  }
}
