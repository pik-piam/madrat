# instead of adding a small number (like 10^-30) to each entry in weight,
# set only those weights to a small number where otherwise data would be lost/
# total sum of the (dis)aggregated object would be different from the original
toolFixWeight <- function(weight, mapping, from, to, dim) {
  # TODO documentation
  # TODO export?
  stopifnot(weight >= 0)
  weightSum <- toolAggregate(weight, mapping, from = from, to = to, dim = dim)
  weightSum[weightSum > 0] <- -Inf
  weightSum[weightSum == 0] <- 10^-30
  newWeight <- toolAggregate(weightSum, mapping, from = to, to = from, dim = dim)

  if (!is.null(getItems(weight, 1))) { # TODO also needed for dim 2 and other cases?
    newWeight <- newWeight[getItems(weight, 1), , ]
  }
  if (!is.null(getItems(weight, 3))) {
    newWeight <- newWeight[, , getItems(weight, 3)]
  }

  newWeight <- pmax(newWeight, weight)
  # TODO add check that weight is actually fixed here or in a test?
  return(newWeight)
}
