# robustOrder, robustSort

robustOrder: A wrapper around base::order that always uses the locale
independent method = "radix". If the argument x is a character vector it
is converted to utf8 first. robustSort: A convenience function using
order to sort a vector using radix sort. The resulting vector will have
the same encoding as the input although internally character vectors are
converted to utf8 before ordering.

## Usage

``` r
robustOrder(..., na.last = TRUE, decreasing = FALSE, method = "radix")
```

## Arguments

- ...:

  One or more vectors of the same length

- na.last:

  If TRUE missing values are put last, if FALSE they are put first, if
  NA they are removed

- decreasing:

  If TRUE decreasing/descending order, if FALSE increasing/ascending
  order. For the "radix" method, this can be a vector of length equal to
  the number of arguments in ... . For the other methods, it must be
  length one.

- method:

  Default is "radix", which is locale independent. The alternatives
  "auto" and "shell" should not be used in madrat because they are
  locale dependent.

## See also

[`order`](https://rdrr.io/r/base/order.html)

## Author

Pascal Sauer
