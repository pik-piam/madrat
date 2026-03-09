# Tool: SplitSubtype

This function can split a subtype string into smaller entities based on
a given separator and check whether these entities exist in a reference
list

## Usage

``` r
toolSplitSubtype(subtype, components, sep = ":")
```

## Arguments

- subtype:

  A character string which can be split with the given separator into
  smaller entities

- components:

  A named list with the same length as the subtype has entities. Names
  of the list are used as names of the entities while the content of
  each list element represents the allowed values of that given entity.
  If all values are allowed use NULL as entry.

- sep:

  separator to be used for splitting

## Value

A named list with the different entities of the given subtype

## Author

Jan Philipp Dietrich

## Examples

``` r
toolSplitSubtype("mymodel:myversion:myworld", list(model=c("mymodel","notmymodel"), 
                                                   version=c("myversion","42"), 
                                                   world="myworld"))
#> $model
#> [1] "mymodel"
#> 
#> $version
#> [1] "myversion"
#> 
#> $world
#> [1] "myworld"
#> 
```
