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
#' @inheritParams toolAggregate
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
toolFixWeight <- function(weight, rel, from, to, dim) {
  stopifnot(weight >= 0, dim %in% 1:3)
  originalDimnames <- dimnames(weight)

  if (is.data.frame(rel)) {
    rel <- unique(rel[, c(from, to)])
    map <- stats::setNames(rel[[from]], rel[[to]])
  } else {
    map <- vapply(rownames(rel), function(i) {
      return(colnames(rel)[rel[i, ] == 1])
    }, character(1))
  }
  stopifnot(setequal(names(map), getItems(weight, dim)))

  # could use add_dimension, but it is much slower
  getItems(weight, dim, full = TRUE, raw = TRUE) <- paste0(getItems(weight, dim, full = TRUE),
                                                           ".",
                                                           map[getItems(weight, dim, full = TRUE)])
  names(dimnames(weight))[dim] <- paste0(names(dimnames(weight))[dim], ".placeholder_dimname")

  # could use magpply's INTEGRATE = TRUE, but that is super slow
  modification <- magpply(weight, max, DIM = dim + 0.1)
  modification <- ifelse(modification == 0, 10^-30, 0)
  modification <- setItems(modification[map, dim = dim],
                           dim, names(map))
  weight <- collapseDim(weight, dim + 0.2)
  weight <- weight + modification

  stopifnot(identical(dimnames(weight), originalDimnames))
  return(weight)
}
