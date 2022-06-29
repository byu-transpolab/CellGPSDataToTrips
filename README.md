
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CellGPSDataToTrips

<!-- badges: start -->
<!-- badges: end -->

The goal of CellGPSDataToTrips is to …

## Project Configuration

This project relies on the tidyverse library. A bug introduced to
[`slice_sample()`](https://github.com/tidyverse/dplyr/issues/6185) means
we need to use an old version of `dplyr`. Install the correct version
with:

``` r
devtools::install_github("tidyverse/dplyr@v1.0.7")
```

Other tidyverse packages are as listed below.

``` r
library(tidyverse)
```

This project is built using the `targets` and `future` libraries.
