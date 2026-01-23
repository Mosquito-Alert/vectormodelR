
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mosquitoR <a href="https://labs.mosquitoalert.com/mosquitoR/"><img src="man/figures/logo.png" align="right" height="139" alt="mosquitoR website" /></a>

<!-- badges: start -->

[![Documentation](https://img.shields.io/static/v1?label=Documentation&message=html&color=informational)](https://labs.mosquitoalert.com/mosquitoR/)
[![R-CMD-check](https://github.com/Mosquito-Alert/mosquitoR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Mosquito-Alert/mosquitoR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of mosquitoR is provide to set of tools for analyzing data
associated with the Mosquito Alert citizen science system
(<https://www.mosquitoalert.com/>), including Mosquito Alert citizen
science data, smart trap data from the Irideon Senscape API, and
traditional mosquito trap data.

IMPORTANT: This package is at an early stage of development, and we may
introduce “breaking” changes. In addition, please note that while the
package contains functions for working with the Irideon Senscape API, it
is not developed by Irideon and is not an official Irideon package.

## Installation

You can install the development version of mosquitoR from
[GitHub](https://github.com/) as follows:

``` r
# install.packages("devtools")
devtools::install_github("Mosquito-Alert/mosquitoR")
```

## Example

Turn a set of latitude and longitude coordinates into the standardized
sampling cell IDs use to organize Mosquito Alert’s anonymous background
tracks:

``` r
library(mosquitoR)
make_samplingcell_ids(lon=c(2.1686, 2.1032), lat=c(41.3874, 41.2098), mask=0.05)
#> [1] "2.15_41.35" "2.1_41.2"
```

Extract longitudes from a set of sampling cell IDs:

``` r
library(mosquitoR)
make_lonlat_from_samplingcell_ids(ids=c("2.15_41.35", "2.10_41.20"), type="lon")
#> [1] "2.15" "2.10"
```

Download smart trap data from the Senscape server using an API key:

``` r
library(mosquitoR)
library(lubridate)
library(dplyr)
library(magrittr)

# first set your API key in your environment (e.g. in ~/.Renviron):
SENSCAPE_API_KEY = Sys.getenv("SENSCAPE_API_KEY")

# get list of device IDs with names that start with ASPB
ASPB_deviceIds = get_senscape_devices(api_key = SENSCAPE_API_KEY) %>% filter(startsWith(name, "ASPB")) %>% pull(`_id`)
ASPB_deviceIds

# get all data from these devices within a specified interval
my_data = get_senscape_data(api_key = SENSCAPE_API_KEY, start_datetime = as_datetime("2023-03-08"), end_datetime = as_datetime("2023-03-09"), deviceIds = ASPB_deviceIds)
my_data
```

## Documentation

Online documentation can be found at
<https://labs.mosquitoalert.com/mosquitoR/>.

## Contributing

If you want to contribute new functions, fix bugs, add documentation or
tests, etc., please do so on the `dev` branch. We are developing this
package using `devtools` and `testthat` and doing our best to follow the
guidelines and styles laid out in <https://r-pkgs.org>.

## Funding

This work has been supported by the Plan Estatal de Investigación
Científica, Técnica y de Innovación 2021-2023 funded by MCIN/AEI/EU -
CNS2022-135646 and by the European Union’s NextGenerationEU/PRTR
program, and by the European Union’s Horizon Europe programme (HORIZON
Research and Innovation Actions) under Grant Agreement 101086640.

<div style="display: flex; gap: 16px; flex-wrap: wrap; align-items: center; margin-top: 12px;">

<img src="man/figures/funding/eu_funded_en.jpg" alt="Funded by the European Union" height="60" />
<img src="man/figures/funding/MICIU+NextG+PRTR+AEI_page-0001.jpg" alt="MCIN/AEI/EU NextGenerationEU/PRTR" height="60" />

</div>

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
