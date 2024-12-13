#' robustOrder, robustSort
#'
#' robustOrder: A wrapper around base::order that always uses the locale independent method = "radix". If the
#' argument x is a character vector it is converted to utf8 first.
#' robustSort: A convenience function using order to sort a vector using radix sort. The resulting vector will have the
#' same encoding as the input although internally character vectors are converted to utf8 before ordering.
#'
#' @param ... One or more vectors of the same length
#' @param na.last If TRUE missing values are put last, if FALSE they are put first, if NA they are removed
#' @param decreasing If TRUE decreasing/descending order, if FALSE increasing/ascending order.
#' For the "radix" method, this can be a vector of length equal to the number of arguments in ... .
#' For the other methods, it must be length one.
#' @param method Default is "radix", which is locale independent. The alternatives "auto" and "shell" should not be used
#' in madrat because they are locale dependent.
#' @seealso \code{\link[base]{order}}
#' @author Pascal Sauer
robustOrder <- function(...,
                        na.last = TRUE, # nolint: object_name_linter.
                        decreasing = FALSE, method = "radix") {
  args <- list(...)
  if (any(lapply(args, length) == 0)) {
    return(vector(mode = "integer", length = 0))
  }
  args <- lapply(args, function(x) {
    if (is.character(x)) {
      return(enc2utf8(x))
    }
    return(x)
  })
  args <- unname(args)
  args$na.last <- na.last
  args$decreasing <- decreasing
  args$method <- method
  return(do.call(order, args))
}

robustSort <- function(x, ...) {
  return(x[robustOrder(x, ...)])
}
