# toolAggregate

(Dis-)aggregates a magclass object from one resolution to another based
on a relation matrix or mapping

## Usage

``` r
toolAggregate(
  x,
  rel,
  weight = NULL,
  from = NULL,
  to = NULL,
  dim = 1,
  wdim = NULL,
  partrel = FALSE,
  negative_weight = "warn",
  mixed_aggregation = FALSE,
  verbosity = 1,
  zeroWeight = "warn"
)
```

## Arguments

- x:

  magclass object that should be (dis-)aggregated

- rel:

  relation matrix, mapping or file containing a mapping in a format
  supported by [`toolGetMapping`](toolGetMapping.md) (currently csv, rds
  or rda). A mapping object consists of any number of columns, where one
  column contains all the elements in x. These elements are mapped to
  the corresponding values in another column, as described below (see
  parameters 'from' and 'to'). It is possible to not set `rel` as long
  as `to` is set and `dim` is chosen appropriately. In that case the
  relation mapping is extracted from the dimnames of the corresponding
  dimension, e.g. if your data contains a spatial subdimension "country"
  you can aggregate to countries via
  `toolAggregate(x, to = "country", dim = 1)`.

- weight:

  magclass object containing weights to be used for a weighted
  (dis)aggregation. The provided weight does not need to be normalized,
  any number \>= 0 is allowed. See "details" section below.

- from:

  Name of source column to be used in rel if it is a mapping (if not set
  the first column matching the data will be used).

- to:

  Name of the target column to be used in rel if it is a mapping (if not
  set the column following column `from` will be used If column `from`
  is the last column, the column before `from is used`). If data should
  be aggregated based on more than one column these columns can be
  specified via "+", e.g. "region+global" if the data should be
  aggregated to column regional as well as column global. In the to
  column, empty values (in the form of "") will not be included in the
  aggregated data (but used as normal aggregation targets). If `rel` is
  missing `to` refers to the aggregation target dimension name.

- dim:

  Specifying the dimension of the magclass object that should be
  (dis-)aggregated. Either specified as an integer
  (1=spatial,2=temporal,3=data) or if you want to specify a sub
  dimension specified by name of that dimension or position within the
  given dimension (e.g. 3.2 means the 2nd data dimension, 3.8 means the
  8th data dimension).

- wdim:

  Specifying the according weight dimension as chosen with dim for the
  aggregation object. If set to NULL the function will try to
  automatically detect the dimension.

- partrel:

  If set to TRUE allows that the relation matrix does contain less
  entries than x and vice versa. These values without relation are lost
  in the output.

- negative_weight:

  Describes how a negative weight should be treated. "allow" means that
  it just should be accepted (dangerous), "warn" returns a warning and
  "stop" will throw an error in case of negative values

- mixed_aggregation:

  boolean which allows for mixed aggregation (weighted mean mixed with
  summations). If set to TRUE weight columns filled with NA will lead to
  summation.

- verbosity:

  Verbosity level of messages coming from the function: -1 = error, 0 =
  warning, 1 = note, 2 = additional information, \>2 = no message

- zeroWeight:

  Describes how a weight sum of 0 for a category/aggregation target
  should be treated. "allow" accepts it and returns 0 (dangerous),
  "setNA" returns NA, "warn" throws a warning, "stop" throws an error,
  "fix" will set zero weights to 10^-30 only where needed, see
  [`toolFixWeight`](toolFixWeight.md).

## Value

the aggregated data in magclass format

## Details

Basically toolAggregate is doing nothing more than a normal matrix
multiplication which is taking into account the 3 dimensional structure
of MAgPIE objects. So, you can provide any kind of relation matrix you
would like. However, for easier usability it is also possible to provide
weights for a weighted (dis-)aggregation as a MAgPIE object. In this
case rel must be a 1-0-matrix or a mapping between both resolutions. The
weight needs to be provided in the higher spatial aggregation, meaning
for aggregation the spatial resolution of your input data and in the
case of disaggregation the spatial resolution of your output data. The
temporal and data dimension must be either identical to the resolution
of the data set that should be (dis-)aggregated or 1. If the temporal
and/or data dimension is 1 this means that the same transformation
matrix is applied for all years and/or all data columns. In the case
that a column should be just summed up instead of being calculated as a
weighted average you either do not provide any weight (then all columns
are just summed up) or your set this specific weighting column to NA and
mixed_aggregation to TRUE.

## See also

[`calcOutput`](calcOutput.md)

## Author

Jan Philipp Dietrich, Ulrich Kreidenweis, Pascal Sauer

## Examples

``` r
# create example mapping
p <- magclass::maxample("pop")
mapping <- data.frame(from = magclass::getItems(p, dim = 1.1),
                      region = rep(c("REG1", "REG2"), 5),
                      global = "GLO")
print(mapping)
#>    from region global
#> 1   AFR   REG1    GLO
#> 2   CPA   REG2    GLO
#> 3   EUR   REG1    GLO
#> 4   FSU   REG2    GLO
#> 5   LAM   REG1    GLO
#> 6   MEA   REG2    GLO
#> 7   NAM   REG1    GLO
#> 8   PAO   REG2    GLO
#> 9   PAS   REG1    GLO
#> 10  SAS   REG2    GLO

# run aggregation
toolAggregate(p, mapping)
#> , , scenario = A2
#> 
#>       t
#> i         y1995   y2005   y2015   y2025   y2035   y2045   y2055
#>   REG1 2234.444 2697.91 3087.15 3513.53 3949.17 4358.10 4730.38
#>   REG2 3238.423 3772.11 4262.33 4749.64 5180.99 5538.65 5824.63
#>       t
#> i        y2065   y2075   y2085   y2095   y2105   y2115   y2125   y2135
#>   REG1 5065.01 5360.94 5609.49 5780.33 5843.42 5843.42 5843.42 5843.42
#>   REG2 6063.32 6258.70 6415.21 6513.97 6542.90 6542.90 6542.90 6542.90
#>       t
#> i        y2145
#>   REG1 5843.42
#>   REG2 6542.90
#> 
#> , , scenario = B1
#> 
#>       t
#> i         y1995   y2005   y2015   y2025   y2035   y2045   y2055
#>   REG1 2234.444 2717.56 3099.14 3429.70 3684.91 3859.21 3953.97
#>   REG2 3238.423 3753.73 4152.68 4469.66 4671.66 4757.63 4730.72
#>       t
#> i        y2065   y2075   y2085   y2095   y2105   y2115   y2125   y2135
#>   REG1 3964.80 3905.82 3782.99 3593.64 3482.03 3482.03 3482.03 3482.03
#>   REG2 4599.84 4388.38 4108.60 3763.25 3574.09 3574.09 3574.09 3574.09
#>       t
#> i        y2145
#>   REG1 3482.03
#>   REG2 3574.09
#> 
# weighted aggregation
toolAggregate(p, mapping, weight = p)
#> , , scenario = A2
#> 
#>       t
#> i          y1995     y2005     y2015     y2025     y2035    y2045
#>   REG1  469.6193  566.4127  664.4549  785.7234  926.5161 1075.828
#>   REG2 1057.3470 1211.8509 1381.6600 1558.2538 1721.4400 1861.314
#>       t
#> i         y2055    y2065    y2075    y2085    y2095    y2105    y2115
#>   REG1 1227.786 1375.802 1508.845 1617.930 1689.104 1713.527 1713.527
#>   REG2 1973.706 2062.114 2123.870 2157.435 2157.161 2147.957 2147.957
#>       t
#> i         y2125    y2135    y2145
#>   REG1 1713.527 1713.527 1713.527
#>   REG2 2147.957 2147.957 2147.957
#> 
#> , , scenario = B1
#> 
#>       t
#> i          y1995     y2005     y2015     y2025    y2035     y2045
#>   REG1  469.6193  573.5739  675.1669  770.5808  849.027  909.0052
#>   REG2 1057.3470 1196.4044 1316.8253 1414.9989 1475.239 1498.1375
#>       t
#> i          y2055     y2065     y2075     y2085     y2095     y2105
#>   REG1  950.8134  967.0437  957.4056  922.7465  865.0403  831.2127
#>   REG2 1485.1098 1436.8647 1359.4952 1256.1984 1129.1512 1060.2820
#>       t
#> i          y2115     y2125     y2135     y2145
#>   REG1  831.2127  831.2127  831.2127  831.2127
#>   REG2 1060.2820 1060.2820 1060.2820 1060.2820
#> 
# combined aggregation across two columns
toolAggregate(p, mapping, to = "region+global")
#> , , scenario = A2
#> 
#>       t
#> i         y1995   y2005   y2015   y2025   y2035   y2045    y2055
#>   REG1 2234.444 2697.91 3087.15 3513.53 3949.17 4358.10  4730.38
#>   REG2 3238.423 3772.11 4262.33 4749.64 5180.99 5538.65  5824.63
#>   GLO  5472.867 6470.02 7349.48 8263.17 9130.16 9896.75 10555.01
#>       t
#> i         y2065    y2075    y2085    y2095    y2105    y2115    y2125
#>   REG1  5065.01  5360.94  5609.49  5780.33  5843.42  5843.42  5843.42
#>   REG2  6063.32  6258.70  6415.21  6513.97  6542.90  6542.90  6542.90
#>   GLO  11128.33 11619.64 12024.70 12294.30 12386.32 12386.32 12386.32
#>       t
#> i         y2135    y2145
#>   REG1  5843.42  5843.42
#>   REG2  6542.90  6542.90
#>   GLO  12386.32 12386.32
#> 
#> , , scenario = B1
#> 
#>       t
#> i         y1995   y2005   y2015   y2025   y2035   y2045   y2055
#>   REG1 2234.444 2717.56 3099.14 3429.70 3684.91 3859.21 3953.97
#>   REG2 3238.423 3753.73 4152.68 4469.66 4671.66 4757.63 4730.72
#>   GLO  5472.867 6471.29 7251.82 7899.36 8356.57 8616.84 8684.69
#>       t
#> i        y2065   y2075   y2085   y2095   y2105   y2115   y2125   y2135
#>   REG1 3964.80 3905.82 3782.99 3593.64 3482.03 3482.03 3482.03 3482.03
#>   REG2 4599.84 4388.38 4108.60 3763.25 3574.09 3574.09 3574.09 3574.09
#>   GLO  8564.64 8294.20 7891.59 7356.89 7056.12 7056.12 7056.12 7056.12
#>       t
#> i        y2145
#>   REG1 3482.03
#>   REG2 3574.09
#>   GLO  7056.12
#> 
```
