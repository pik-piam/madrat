#' Tool: regionscode
#'
#' Given a regionmapping (mapping between ISO countries and regions) the
#' function calculates a regionscode which is basically the md5sum of a reduced
#' form of the mapping. The regionscode is unique for each regionmapping and
#' can be used to clearly identify a given regionmapping. In addition several
#' checks are performed to make sure that the given input is a proper
#' regionmapping
#'
#'
#' @param mapping Either a path to a mapping or an already read-in mapping as
#' data.frame. If set to NULL (default) the regionscode of the region mapping set in the
#' madrat config will be returned.
#' @param label logical deciding whether the corresponding label of a regionscode
#' should be returned instead of the regionscode.
#' @param strict If set to TRUE region mappings with mapping to ISO countries with exactly 2 columns
#' or more than 2 colums (if the first colum contains irrelevant information which will be deleted automatically)
#' will be accepted. In this case data will be transformed and even cases with different ordering
#' will yield the same regionscode. If set to FALSE all these checks will be ignored and the
#' regionscode will be just computed on the object as it is. Please be aware the regionscode will
#' differ with strict mode on or off!
#' @return A md5-based regionscode which describes the given mapping or, if \code{label=TRUE}
#' and a corresponding label is available, the label belonging to the regionscode
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{toolCodeLabels}}, \code{\link{fingerprint}}, \code{\link[digest]{digest}}
#' @examples
#'
#' file <- system.file("extdata", "regionmappingH12.csv", package = "madrat")
#' regionscode(file)
#' @importFrom digest digest
#' @export
regionscode <- function(mapping = NULL, label = FALSE, strict = TRUE) {

  if (is.null(mapping)) mapping <- c(getConfig("regionmapping"), getConfig("extramappings"))

  setWrapperInactive("wrapperChecks")

  if (is.character(mapping)) {
    if (length(mapping) > 1) {
      return(sapply(mapping, regionscode, label = label, strict = strict)) # nolint
    }
    mapping <- toolGetMapping(mapping, "regional")
  }

  row.names(mapping) <- NULL

  if (strict) {
    # remove first column if data has 3 or more columns
    if (ncol(mapping) >= 3) mapping[[1]] <- NULL

    # read list of ISO-countries
    isCountry  <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
    isCountry1 <- as.vector(isCountry[, "x"])
    names(isCountry1) <- isCountry[, "X"]
    isocountries <- robustSort(isCountry1)

    if (nrow(mapping) > length(isocountries)) {
      stop("Provided regionmapping has more rows than there are ISO countries in the ISO reference list. ",
           "Please check the mapping!")
    }
    if (nrow(mapping) < length(isocountries)) {
      stop("Provided regionmapping has less rows than there are ISO countries in the ISO reference list. ",
           "Please check the mapping!")
    }

    listsAgree <- NULL
    for (i in seq_len(ncol(mapping))) {
      listsAgree <- c(listsAgree, all(isocountries == robustSort(as.vector(mapping[[i]]))))
    }

    if (!any(listsAgree)) {
      stop("Provided regionmapping does not contain a iso country column which agrees with the reference ",
           "list of ISO countries! Please check the mapping!")
    }

    # Reorder if only second column contains ISO countries
    if (!listsAgree[1]) mapping <- mapping[2:1]

    tmp <- as.vector(mapping[[1]])
    for (i in 2:ncol(mapping)) {
      tmp <- robustSort(paste(tmp, as.vector(mapping[[i]]), sep = "."))
    }
  } else {
    tmp <- mapping
  }
  out <- digest(tmp, algo = getConfig("hash"))

  setWrapperActive("wrapperChecks")

  if (label) {
    return(toolCodeLabels(out))
  }
  return(out)
}
