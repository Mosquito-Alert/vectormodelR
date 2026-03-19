# Aggregates mosquito alert report data by country or city

Aggregates mosquito alert report data by country or city

## Usage

``` r
get_malert_aggregates(aggregate_type, filter_year, country_code, file_layer)
```

## Arguments

- aggregate_type:

  String. The type of aggregation to perform. Options are "country" or
  "city".

- filter_year:

  String. The year(s) to filter the data. Options include a single year
  (e.g., "2014"), a comma-separated list of years (e.g., "2014,2015"),
  or a range (e.g., "2014-2016"). Defaults to NULL (all years).

- country_code:

  String. The ISO country code (required if aggregate_type is "city").

- file_layer:

  Integer. The layer of the shapefile/geopackage to access (for city
  aggregation).

## Value

A data frame containing the aggregated mosquito alert report counts.

## Examples

``` r
# Aggregate mosquito reports by country for 2015, 2016 and 2018
get_malert_aggregates(aggregate_type = "country", filter_year = "2015,2016,2018")
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
#> # A tibble: 22 × 2
#>    country count
#>    <chr>   <int>
#>  1 ESP     10247
#>  2 NA        464
#>  3 ITA        32
#>  4 FRA        18
#>  5 ECU         7
#>  6 BEL         6
#>  7 PRT         6
#>  8 NLD         5
#>  9 LKA         4
#> 10 GBR         3
#> # ℹ 12 more rows

# Aggregate mosquito reports by city for Spain in 2014-2024
get_malert_aggregates(aggregate_type = "city", filter_year = "2014-2024", country_code = "ESP", file_layer = 2)
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
#> Reading layer `ADM_ADM_2' from data source `/tmp/RtmpEud9px/file203e3fd04596.gpkg' using driver `GPKG'
#> Simple feature collection with 52 features and 13 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -18.16153 ymin: 27.63736 xmax: 4.328195 ymax: 43.79153
#> Geodetic CRS:  WGS 84
#> Simple feature collection with 53 features and 2 fields
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: -17.98223 ymin: 27.74339 xmax: 4.300591 ymax: 43.73795
#> Geodetic CRS:  WGS 84
#> # A tibble: 53 × 3
#>    NAME_2    count                                                      geometry
#>    <chr>     <int>                                              <MULTIPOINT [°]>
#>  1 Barcelona 33751 ((1.922108 41.25949), (1.922147 41.25953), (1.922903 41.2597…
#>  2 Valencia   9928 ((-1.303959 40.1301), (-1.304626 40.13287), (-1.286447 40.06…
#>  3 Alicante   6892 ((-0.8738577 38.67293), (-0.8744169 38.67128), (-0.7764138 3…
#>  4 Madrid     5993 ((-4.43173 40.30465), (-4.469095 40.31342), (-4.463177 40.31…
#>  5 Girona     5585 ((2.393738 41.84666), (2.439038 41.85592), (2.345646 41.8787…
#>  6 Baleares   5564 ((4.279681 39.81185), (4.279703 39.81185), (4.245101 39.8214…
#>  7 Málaga     5369 ((-5.252121 36.31937), (-5.252121 36.31937), (-5.251944 36.3…
#>  8 Sevilla    4958 ((-5.445939 37.12482), (-5.451539 37.12747), (-5.45576 37.12…
#>  9 Murcia     3951 ((-0.9029761 37.87323), (-0.8983285 37.82926), (-0.9010854 3…
#> 10 Castellón  3631 ((-0.6990481 40.01869), (-0.124828 40.65912), (-0.05694531 4…
#> # ℹ 43 more rows
```
