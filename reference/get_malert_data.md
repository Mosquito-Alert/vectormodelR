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
  desired_cols = NULL,
  filters = NULL
)
```

## Arguments

- source:

  String. Source to download from. Options are "github" or "zenodo".

- doi:

  String. Zenodo DOI if downloading from Zenodo. Default is the DOI that
  always points to the most recent version: 10.5281/zenodo.597466.

- iso3:

  Optional three-letter ISO code used for spatial filtering.

- admin_level:

  Optional administrative level used for spatial filtering.

- admin_name:

  Optional administrative unit name. When `NULL`, all administrative
  units at `admin_level` are included.

- desired_cols:

  Optional character vector (or list) of column names to retain after
  spatial filtering. When `NULL`, all available columns are kept.

- filters:

  Optional named list of values to filter by. Each element name should
  correspond to a column in the dataset, and the value describes what to
  keep (e.g. `list(type = "adult")`).

## Value

A tibble of Mosquito Alert reports.

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
