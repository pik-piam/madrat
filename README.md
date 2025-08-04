# May All Data be Reproducible and Transparent (MADRaT) *

R package **madrat**, version **3.23.1**

[![CRAN status](https://www.r-pkg.org/badges/version/madrat)](https://cran.r-project.org/package=madrat) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1115490.svg)](https://doi.org/10.5281/zenodo.1115490) [![R build status](https://github.com/pik-piam/madrat/workflows/check/badge.svg)](https://github.com/pik-piam/madrat/actions) [![codecov](https://codecov.io/gh/pik-piam/madrat/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/madrat) [![r-universe](https://pik-piam.r-universe.dev/badges/madrat)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Provides a framework which should improve reproducibility and
    transparency in data processing. It provides functionality such as
    automatic meta data creation and management, rudimentary quality
    management, data caching, work-flow management and data aggregation.
    * The title is a wish not a promise. By no means we expect this
    package to deliver everything what is needed to achieve full
    reproducibility and transparency, but we believe that it supports
    efforts in this direction.


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

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("madrat-caching") # Data caching in madrat
vignette("madrat-puc")     # Portable unagreggated collections (PUCs) in MADRaT
vignette("madrat")         # Data preparation with madrat
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **madrat** in publications use:

Dietrich J, Sauer P, Baumstark L, Wirth S, Giannousakis A, Rodrigues R, Bodirsky B, Leip D, Kreidenweis U, Klein D, Rein P (2025). "madrat: May All Data be Reproducible and Transparent (MADRaT) *." doi:10.5281/zenodo.1115490 <https://doi.org/10.5281/zenodo.1115490>, Version: 3.23.1, <https://github.com/pik-piam/madrat>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {madrat: May All Data be Reproducible and Transparent (MADRaT) *},
  author = {Jan Philipp Dietrich and Pascal Sauer and Lavinia Baumstark and Stephen Wirth and Anastasis Giannousakis and Renato Rodrigues and Benjamin Leon Bodirsky and Debbora Leip and Ulrich Kreidenweis and David Klein and Patrick Rein},
  doi = {10.5281/zenodo.1115490},
  date = {2025-08-08},
  year = {2025},
  url = {https://github.com/pik-piam/madrat},
  note = {Version: 3.23.1},
}
```
