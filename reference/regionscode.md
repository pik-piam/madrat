# Tool: regionscode

Given a regionmapping (mapping between ISO countries and regions) the
function calculates a regionscode which is basically the md5sum of a
reduced form of the mapping. The regionscode is unique for each
regionmapping and can be used to clearly identify a given regionmapping.
In addition several checks are performed to make sure that the given
input is a proper regionmapping

## Usage

``` r
regionscode(mapping = NULL, label = FALSE, strict = TRUE)
```

## Arguments

- mapping:

  Either a path to a mapping or an already read-in mapping as
  data.frame. If set to NULL (default) the regionscode of the region
  mapping set in the madrat config will be returned.

- label:

  logical deciding whether the corresponding label of a regionscode
  should be returned instead of the regionscode.

- strict:

  If set to TRUE region mappings with mapping to ISO countries with
  exactly 2 columns or more than 2 colums (if the first colum contains
  irrelevant information which will be deleted automatically) will be
  accepted. In this case data will be transformed and even cases with
  different ordering will yield the same regionscode. If set to FALSE
  all these checks will be ignored and the regionscode will be just
  computed on the object as it is. Please be aware the regionscode will
  differ with strict mode on or off!

## Value

A md5-based regionscode which describes the given mapping or, if
`label=TRUE` and a corresponding label is available, the label belonging
to the regionscode

## See also

[`toolCodeLabels`](toolCodeLabels.md), [`fingerprint`](fingerprint.md),
[`digest`](https://eddelbuettel.github.io/digest/man/digest.html)

## Author

Jan Philipp Dietrich

## Examples

``` r
file <- system.file("extdata", "regionmappingH12.csv", package = "madrat")
regionscode(file)
#> [1] "62eff8f7"
```
