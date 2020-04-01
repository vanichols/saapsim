
<!-- README.md is generated from README.Rmd. Please edit that file -->

# saapsim

<!-- badges: start -->

<!-- badges: end -->

The goal of saapsim is to provide access to commonly needed functions
when working with APSIM (for the Archontoulis Lab).

## Installation

The development version is on [GitHub](https://github.com/), accessed
with

``` r
#install.packages("devtools")
#devtools::install_github("vanichols/saapsim")
```

## Example(s)

All functions start with saf\_xxx (sotris apsim function), so you can
easily see a list pop up if you type saf and hit tab.

You can read in ‘.out’ files and smoosh them into one dataframe with the
*saf\_readapout* function

``` r
#library(saapsim)
#mydir <- "/apsim_results_folder"
#saf_readapout(mydir)
```

You can easily convert corn bushes/acre to dry kg/ha with the
*saf\_buac\_to\_kgha\_corn* function

``` r
library(saapsim)
saf_buac_to_kgha_corn(210)
#> [1] 11204.44
```
