# toolCountry2isocode

Function used to convert country names from the long name to the ISO
3166-1 alpha 3 country code

## Usage

``` r
toolCountry2isocode(
  country,
  warn = TRUE,
  ignoreCountries = NULL,
  type = NULL,
  mapping = NULL
)
```

## Arguments

- country:

  A vector of country names

- warn:

  whether warnings should be printed now or in the end of the whole
  process as notes

- ignoreCountries:

  A vector of country names/codes that exist in the data and that should
  be removed but without creating a warning (they will be removed in any
  case). You should use that argument if you are certain that the given
  entries should be actually removed from the data.

- type:

  deprecated and will be removed soon!

- mapping:

  additional mappings as a names vector

## Value

the ISO 3166-1 alpha 3 country code

## See also

[`readSource`](readSource.md),[`getSources`](getSources.md)

## Author

Jan Philipp Dietrich, Anastasis Giannousakis

## Examples

``` r
toolCountry2isocode("Germany")
#> [1] "DEU"
toolCountry2isocode(c("Germany","Fantasyland"),mapping=c("Fantasyland"="BLA"))
#> [1] "DEU" "BLA"
```
