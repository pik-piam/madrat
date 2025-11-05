# instead of adding a small number (like 10^-30) to each entry in weight,
# set only those weights to a small number where otherwise data would be lost/
# total sum of the (dis)aggregated object would be different from the original
toolFixWeight <- function(weight, mapping, from, to, dim) {
  stopifnot(weight >= 0)
  weightSum <- toolAggregate(weight, mapping, from = from, to = to, dim = dim)
  weightSum[weightSum > 0] <- -Inf
  weightSum[weightSum == 0] <- 10^-30
  newWeight <- toolAggregate(weightSum, mapping, from = to, to = from, dim = dim)
  newWeight <- newWeight[getItems(weight, 1), , getItems(weight, 3)]
  newWeight <- pmax(newWeight, weight)
  return(newWeight)
}
