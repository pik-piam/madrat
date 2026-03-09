# metadataGFZ

Function to extract metadata information of a data set hosted at GFZ
dataservices (https://dataservices.gfz-potsdam.de/portal/).

## Usage

``` r
metadataGFZ(doi)
```

## Arguments

- doi:

  DOI of a data set hosted at GFZ dataservices

## Value

a list with entries "license", "citation", "authors" and "year"

## See also

[`toolstartmessage`](toolstartmessage.md), [`vcat`](vcat.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
metadataGFZ("10.5880/pik.2019.004")
} # }
```
