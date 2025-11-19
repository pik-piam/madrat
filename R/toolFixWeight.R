#' toolFixWeight
#'
#' Check the example below for the problem this function is solving.
#' Instead of adding a small number (like 10^-10) to each entry in weight,
#' set only those weights to 10^-30 where otherwise the total sum of the
#' (dis)aggregated object would be different from the original.
#'
#' @param weight magclass object containing weights to be used for a weighted
#' (dis)aggregation. The provided weight does not need to be normalized, any
#' number >= 0 is allowed.
#' @param map a data frame where the first column contains coarse resolution items
#' and the second column contains fine resolution items; fine resolution items
#' must match items in weight
#' @param dim which dim to fix: 1, 2, or 3
#' @return weight, with weights set to 10^-30 only where otherwise the total
#' sum of the (dis)aggregated object would be different from the original
#'
#' @examples
#' x <- new.magpie(c("A", "B"), fill = 100)
#' rel <- data.frame(from = c("A", "A", "B", "B"),
#'                   to = c("A1", "A2", "B1", "B2"))
#' weight <- new.magpie(c("A1", "A2", "B1", "B2"), fill = 0)
#' weight["B1", , ] <- 1
#'
#' y <- toolAggregate(x, rel, weight) # warning "Weight sum is 0"
#' stopifnot(sum(x) - sum(y) == 100) # total sum no longer equal, hence the warning
#' stopifnot(as.vector(y["B2", , ]) == 0)
#'
#' # problematic hotfix
#' y <- toolAggregate(x, rel, weight + 10^-10) # no warning
#' stopifnot(sum(x) == sum(y)) # total sum is equal
#' stopifnot(as.vector(y["B2", , ]) > 0) # this is the problem, should still be 0
#'
#' # proper fix
#' y <- toolAggregate(x, rel, weight, zeroWeight = "fix") # no warning
#' stopifnot(sum(x) == sum(y)) # total sum is equal
#' stopifnot(as.vector(y["B2", , ]) == 0) # and this is also still 0
#'
#' @seealso \code{\link{toolAggregate}}
#' @author Pascal Sauer
#' @export
toolFixWeight <- function(weight, map, dim) {
  stopifnot(weight >= 0,
            setequal(map[[2]], getItems(weight, dim)))
  originalDimnames <- dimnames(weight)

  extramap <- NULL
  map <- unique(map[, 1:2])
  if (dim %in% 1:3) {
    if (ndim(weight, dim) == 1) {
      stopifnot(!grepl(".", map, fixed = TRUE))
    } else {
      # merge all subdims into one dim, and replace back at the end using extramap
      originalMap2 <- map[[2]]
      map[[1]] <- sub(".", "p", map[[1]], fixed = TRUE)
      map[[2]] <- sub(".", "p", map[[2]], fixed = TRUE)
      extramap <- stats::setNames(nm = map[[2]], originalMap2)
      getItems(weight, dim, full = TRUE) <- sub(".", "p", getItems(weight, dim, full = TRUE), fixed = TRUE)
    }
    dim <- dim + 0.1
  }
  map <- stats::setNames(nm = map[[2]], object = map[[1]])

  # add subdim for coarse items according to map
  # could use add_dimension, but it is much slower
  getItems(weight, floor(dim), full = TRUE, raw = TRUE) <- paste0(getItems(weight, floor(dim), full = TRUE),
                                                                  ".",
                                                                  map[getItems(weight, dim, full = TRUE)])
  names(dimnames(weight))[floor(dim)] <- paste0(names(dimnames(weight))[floor(dim)], ".placeholder_dimname")

  # determine all coarse items where all values are zero (by checking max == 0)
  # could use magpply's INTEGRATE = TRUE, but that is super slow
  modification <- magpply(weight, max, DIM = dim)

  # for coarse items where all weights are zero set modification to 10^-30, all others need no modification
  modification <- ifelse(modification == 0, 10^-30, 0)
  modification <- setItems(modification[map, dim = floor(dim)],
                           dim, names(map))
  weight <- collapseDim(weight, floor(dim) + 0.1 * ndim(weight, floor(dim)))
  stopifnot(sameDims(modification, weight))
  weight <- weight + modification

  if (!is.null(extramap)) {
    getItems(weight, floor(dim), full = TRUE, raw = TRUE) <- extramap[getItems(weight, floor(dim), full = TRUE)]
    names(dimnames(weight))[floor(dim)] <- names(originalDimnames)[floor(dim)]
  }

  stopifnot(identical(dimnames(weight), originalDimnames))
  return(weight)
}
