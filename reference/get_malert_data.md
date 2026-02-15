# Download Mosquito Alert report data from GitHub or Zenodo

Download Mosquito Alert report data from GitHub or Zenodo

## Usage

``` r
get_malert_data(
  source = "zenodo",
  doi = "10.5281/zenodo.597466",
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  desired_cols = NULL
)
```

## Arguments

- source:

  String. Source to download from. Options are "github" or "zenodo".

- doi:

  String. Zenodo DOI if downloading from Zenodo. Default is the DOI that
  will always point to the most recent version: 10.5281/zenodo.597466.

- iso3:

  Optional three-letter ISO code used to locate a perimeter file for
  spatial filtering.

- admin_level:

  Optional administrative level associated with the perimeter file.

- admin_name:

  Optional administrative unit name associated with the perimeter file.

- desired_cols:

  Optional character vector (or list) of column names to retain after
  spatial filtering. When `NULL`, all available columns are kept.

  The function always writes the raw combined download to
  `data/vector/vector_global_malert.Rds`. When a perimeter is supplied,
  the filtered output (after column selection) is persisted to
  `data/proc/vector_<iso3>_<admin_level>_<admin_name>_malert.Rds`. When
  spatial filtering is applied, records are further restricted to
  `type == "adult"` when that column is available.

## Value

A tibble of Mosquito Alert reports (filtered and column-selected if the
optional perimeter inputs are supplied).

## Examples

``` r
# Download raw data without filtering
malert_reports <- get_malert_data(source = "github")
#> Loading year: 2014
#> Loading year: 2015
#> Loading year: 2016
#> Loading year: 2017
#> Loading year: 2018
#> Loading year: 2019
#> Loading year: 2020
#> Loading year: 2021
#> Loading year: 2022
#> Loading year: 2023
#> Loading year: 2024
#> Loading year: 2025
#> Loading year: 2026
#> Saved raw Mosquito Alert reports to data/vector/vector_global_malert.Rds
```
