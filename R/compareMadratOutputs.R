#' Compare a madrat function's output with and without your changes
#'
#' With `compareMadratOutputs` you can easily compare the output of a madrat function (read, calc, ...) with and without
#' your changes. First, run `compareMadratOutputs` without your changes, so a `.rds` file with the original output is
#' saved. Then apply your changes and run `compareMadratOutputs` again to compare the new output to the original output.
#'
#' If there are differences a `<functionName>-new.rds` containing the new output is saved for closer inspection. All
#' files are created in the current working directory.
#'
#' @param package [character(1)] The package where the given function is located. It will be attached via `library`.
#' @param functionName [character(1)] The name of the function from which you want to compare outputs. Must be a madrat
#' function whose name starts with read, correct, convert, or calc.
#' @param subtypes [character(n)] The subtypes you want to check. For calc functions this must be NULL.
#' @param overwriteOld If TRUE: overwrite a "*-old-*.rds" previously created with compareMadratOutputs.
#' @return Invisibly the result of `waldo::compare` or `all.equal` if a comparison was made, otherwise a named list of
#' the outputs for each subtype.
#'
#' @examples
#' \dontrun{
#' # save original output to readTau-old.rds
#' compareMadratOutputs("madrat", "readTau", c("paper", "historical"))
#'
#' # now apply your changes to madrat:::readTau, reinstall madrat, restart the R session
#'
#' # compare new output to original output from readTau-old.rds
#' compareMadratOutputs("madrat", "readTau", c("paper", "historical"))
#' }
#'
#' @author Pascal Sauer
#'
#' @importFrom digest digest
#' @importFrom magclass where
#' @importFrom utils askYesNo
#' @export
compareMadratOutputs <- function(package, functionName, subtypes, overwriteOld = FALSE) {
  oldRds <- Sys.glob(paste0(functionName, "-old-*.rds"))
  stopifnot(length(oldRds) %in% 0:1)

  if (overwriteOld) {
    unlink(oldRds)
    oldRds <- character(0)
  } else {
    question <- paste0("Are you using the original, unchanged ", functionName, " right now?")
    if ((length(oldRds) == 0 && !askYesNo(question))) {
      stop("You need to run compareMadratOutputs with the original, unchanged ", functionName,
           " first, so you can compare the output of your changed version with the original output.")
    }
  }

  functionHash <- digest(eval(str2expression(paste0(package, ":::", functionName))), "xxhash32")
  if (length(oldRds) == 1 && sub(paste0("^", functionName, "-old-(.+)[.]rds$"), "\\1", oldRds) == functionHash) {
    stop("Your are using the same version of ", functionName, " that ", oldRds, " was created with. ",
         "Please apply your changes, re-install ", package, " with your changes, and restart the R session.")
  }

  message("Running library(", package, ")")
  library(package, character.only = TRUE) # nolint
  localConfig(ignorecache = functionName)
  if (is.null(subtypes)) {
    subtypes <- list(NULL)
  }
  output <- lapply(subtypes, function(subtype) {
    if (startsWith(functionName, "read")) {
      return(readSource(sub("^read", "", functionName), subtype = subtype, convert = FALSE))
    } else if (startsWith(functionName, "correct")) {
      return(readSource(sub("^correct", "", functionName), subtype = subtype, convert = "onlycorrect"))
    } else if (startsWith(functionName, "convert")) {
      return(readSource(sub("^convert", "", functionName), subtype = subtype, convert = TRUE))
    } else if (startsWith(functionName, "calc")) {
      stopifnot(is.null(subtype))
      return(calcOutput(sub("^calc", "", functionName), aggregate = FALSE))
    } else {
      stop(functionName, " does not start with read, correct, convert, or calc.")
    }
  })
  names(output) <- subtypes

  if (length(oldRds) == 1) {
    oldOutput <- readRDS(oldRds)
    if (identical(oldOutput, output)) {
      message("no differences")
    } else {
      saveRDS(output, paste0(functionName, "-new.rds"))
      message("Saved '", functionName, "-new.rds'. Found some differences:")
      comparison <- lapply(seq_along(output), function(i) {
        return(where(oldOutput[[1]] != output[[1]])[["true"]][-1])
      })
      names(comparison) <- subtypes
      print(comparison)
      return(invisible(comparison))
    }
  } else {
    oldRds <- paste0(functionName, "-old-", functionHash, ".rds")
    saveRDS(output, oldRds)
    message("Saved '", oldRds, "'. Now apply your changes to ", functionName, ", re-install ", package, ", restart R, ",
            "and re-run compareMadratOutputs to compare the output of your changed version to the original output.")
    return(invisible(output))
  }
}
