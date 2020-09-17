# May All Data be Reproducible and Transparent (MADRaT) *

R package **madrat**, version **1.83.3**

[![Travis build status](https://travis-ci.com/pik-piam/madrat.svg?branch=master)](https://travis-ci.com/pik-piam/madrat) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1115490.svg)](https://doi.org/10.5281/zenodo.1115490) [![codecov](https://codecov.io/gh/pik-piam/madrat/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/madrat)

## Purpose and Functionality

Provides a framework which should improve reproducibility and transparency in data processing. It provides functionality such as automatic meta data creation and management, rudimentary quality management, data caching, work-flow management and data aggregation.
    * The title is a wish not a promise. By no means we expect this package to deliver everything what is needed to achieve full reproducibility and transparency, but we believe that it supports efforts in this direction.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("madrat")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("madrat") # Data preparation with MADRaT
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **madrat** in publications use:

Dietrich J, Baumstark L, Wirth S, Giannousakis A, Rodrigues R, Bodirsky B, Kreidenweis U (2020). _madrat: May All Data
be Reproducible and Transparent (MADRaT)_. doi: 10.5281/zenodo.1115490 (URL: https://doi.org/10.5281/zenodo.1115490), R
package version 1.83.3, <URL: https://github.com/pik-piam/madrat>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {madrat: May All Data be Reproducible and Transparent (MADRaT)},
  author = {Jan Philipp Dietrich and Lavinia Baumstark and Stephen Wirth and Anastasis Giannousakis and Renato Rodrigues and Benjamin Leon Bodirsky and Ulrich Kreidenweis},
  year = {2020},
  note = {R package version 1.83.3},
  doi = {10.5281/zenodo.1115490},
  url = {https://github.com/pik-piam/madrat},
}
```

