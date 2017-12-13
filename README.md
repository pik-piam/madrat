# madrat  | May All Data be Reproducible and Transparent <sup>1</sup>
R package

## Purpose and Functionality

MADRaT provides a framework which should improve reproducibility and transparency in data processing. It provides functionality such as automatic meta data creation and management, rudimentary quality management, data caching, work-flow management and data aggregation. 

## Installation

The last official release of the package can be directly installed from official CRAN repository:

```r 
install.packages("madrat")
```

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

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
vignette("madrat")
```

## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de>.

<sup>1</sup> The title is a wish not a promise. By no means we expect this package to deliver everything what is needed to achieve full reproducibility and transparency, but we believe that it supports efforts in this direction. 

## Citation

[![DOI](https://zenodo.org/badge/92809958.svg)](https://zenodo.org/badge/latestdoi/92809958)

