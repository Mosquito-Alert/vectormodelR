# Retrieves mosquito alert reports with geopackage integration

Retrieves mosquito alert reports with geopackage integration

## Usage

``` r
get_malert_geopackage_data(filter_year, country_code, file_layer)
```

## Arguments

- filter_year:

  String. The year(s) to filter the data. Options include a single year
  (e.g., "2014"), a comma-separated list (e.g., "2014,2015"), or a range
  (e.g., "2014-2016"). Defaults to NULL (all years).

- country_code:

  String. The ISO country code for the desired country.

- file_layer:

  Integer. The layer of the geopackage to access. Defaults to the last
  available layer if not specified.

## Value

A spatial data frame containing the mosquito alert reports joined with
geopackage data.

## Examples

``` r
# Retrieve data for mosquito alerts in Spain for 2014-2024
get_malert_geopackage_data(filter_year = "2014-2024", country_code = "ESP", file_layer = 4)
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
#> Reading layer `ADM_ADM_4' from data source `/tmp/Rtmpix0yF2/file1d17184bf3.gpkg' using driver `GPKG'
#> Simple feature collection with 8302 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -18.16153 ymin: 27.63736 xmax: 4.328195 ymax: 43.79153
#> Geodetic CRS:  WGS 84
#> Simple feature collection with 114919 features and 135 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -17.98223 ymin: 27.74339 xmax: 4.300591 ymax: 43.73795
#> Geodetic CRS:  WGS 84
#> # A tibble: 114,919 × 136
#>    version_UUID creation_time creation_date creation_day_since_l…¹ creation_year
#>  * <chr>        <chr>         <chr>                          <int>         <int>
#>  1 4abc98e5-6d… 2014-09-01T0… 2014-09-01                        80          2014
#>  2 4b02c972-af… 2014-08-28T1… 2014-08-28                        76          2014
#>  3 EB958A91-4F… 2014-08-28T0… 2014-08-28                        76          2014
#>  4 D30B7A1E-BB… 2014-08-09T0… 2014-08-09                        57          2014
#>  5 020988f1-b6… 2014-10-16T1… 2014-10-16                       125          2014
#>  6 ccb72c3d-50… 2014-10-26T0… 2014-10-26                       135          2014
#>  7 021111b2-dd… 2014-08-12T0… 2014-08-12                        60          2014
#>  8 021572D8-46… 2014-09-08T2… 2014-09-08                        87          2014
#>  9 0272524f-76… 2014-10-05T1… 2014-10-05                       114          2014
#> 10 02a23726-97… 2014-06-24T1… 2014-06-24                        11          2014
#> # ℹ 114,909 more rows
#> # ℹ abbreviated name: ¹​creation_day_since_launch
#> # ℹ 131 more variables: creation_month <int>, site_cat <int>, type <chr>,
#> #   location_is_masked <lgl>, tigaprob_cat <int>, latest_version <lgl>,
#> #   visible <lgl>, n_photos <int>, final_expert_status_text <int>,
#> #   responses <list>, country <chr>, updated_at <chr>,
#> #   datetime_fix_offset <int>, nuts_2 <chr>, nuts_3 <chr>, …
```
