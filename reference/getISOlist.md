# get official ISO list

Function which returns the ISO list which is used as default for the
input data preparation. It contains the countries to which all source
data has to be up- or downscaled to.

## Usage

``` r
getISOlist(type = "all", threshold = 1)
```

## Arguments

- type:

  Determines what countries should be returned. "all" returns all
  countries, "important" returns all countries which are above the
  population threshold set in the configuration and "dispensable"
  returns all countries which are below the threshold.

- threshold:

  Population threshold in million capita which determines whether the
  country is put into the "important" or "dispensable" class (default =
  1 mio. people)

## Value

vector of default ISO country codes.

## Note

Please always use this function instead of directly referring to the
data object as the format in this data list might change in the future!

## See also

[`getSources`](getSources.md), [`getCalculations`](getCalculations.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
head(getISOlist())
#>         Aruba   Afghanistan        Angola      Anguilla Aland Islands 
#>         "ABW"         "AFG"         "AGO"         "AIA"         "ALA" 
#>       Albania 
#>         "ALB" 
head(getISOlist("dispensable"))
#>          Aruba       Anguilla  Aland Islands        Andorra 
#>          "ABW"          "AIA"          "ALA"          "AND" 
#> American Samoa     Antarctica 
#>          "ASM"          "ATA" 
```
