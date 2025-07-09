
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CEIDR

<!-- badges: start -->
<!-- badges: end -->

The goal of CEIDR is to …

## Installation

You can install the development version of CEIDR like so:

``` r
# install.packages("devtools")
# devtools::install_github("RuyiPan/CEIDR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CEIDR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
m1 <- exampleData$thickness_lh
m2 <- exampleData$sucl_lh

test <- exampleData$thickness_lh
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
