# Retrieves geopackage integration from GADM (https://gadm.org)

Retrieves geopackage integration from GADM (https://gadm.org)

## Usage

``` r
get_geopackage_data(country_code, file_layer)
```

## Arguments

- country_code:

  String. The ISO country code for the desired country.

- file_layer:

  Integer. The layer of the geopackage to access. Defaults to last
  available layer.

## Value

The geopackage for the specified country and layer.

## Examples

``` r
# Retrieve geopackage data for Spain (ESP) and layer 4
get_geopackage_data(country_code = "ESP", file_layer = 4)
#> Reading layer `ADM_ADM_4' from data source `/tmp/Rtmpix0yF2/file1d1768418516.gpkg' using driver `GPKG'
#> Simple feature collection with 8302 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -18.16153 ymin: 27.63736 xmax: 4.328195 ymax: 43.79153
#> Geodetic CRS:  WGS 84
#> Simple feature collection with 8302 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -18.16153 ymin: 27.63736 xmax: 4.328195 ymax: 43.79153
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>             GID_4 GID_0 COUNTRY   GID_1    NAME_1     GID_2  NAME_2       GID_3
#> 1   ESP.1.1.1.1_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 2   ESP.1.1.1.2_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 3   ESP.1.1.1.3_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 4   ESP.1.1.1.4_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 5   ESP.1.1.1.5_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 6   ESP.1.1.1.6_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 7   ESP.1.1.1.7_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 8   ESP.1.1.1.8_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 9   ESP.1.1.1.9_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#> 10 ESP.1.1.1.10_1   ESP   Spain ESP.1_1 Andalucía ESP.1.1_1 Almería ESP.1.1.1_1
#>       NAME_3              NAME_4 VARNAME_4       TYPE_4    ENGTYPE_4 CC_4
#> 1  n.a. (15)           Albánchez        NA Municipality Municipality   NA
#> 2  n.a. (15)               Albox        NA Municipality Municipality   NA
#> 3  n.a. (15)            Alcóntar        NA Municipality Municipality   NA
#> 4  n.a. (15)            Arboleas        NA Municipality Municipality   NA
#> 5  n.a. (15) Armuña de Almanzora        NA Municipality Municipality   NA
#> 6  n.a. (15)             Bacares        NA Municipality Municipality   NA
#> 7  n.a. (15)            Bayarque        NA Municipality Municipality   NA
#> 8  n.a. (15)            Cantoria        NA Municipality Municipality   NA
#> 9  n.a. (15)             Chercos        NA Municipality Municipality   NA
#> 10 n.a. (15)               Fines        NA Municipality Municipality   NA
#>                              geom
#> 1  MULTIPOLYGON (((-2.202082 3...
#> 2  MULTIPOLYGON (((-2.056954 3...
#> 3  MULTIPOLYGON (((-2.60811 37...
#> 4  MULTIPOLYGON (((-2.131044 3...
#> 5  MULTIPOLYGON (((-2.419804 3...
#> 6  MULTIPOLYGON (((-2.477338 3...
#> 7  MULTIPOLYGON (((-2.42154 37...
#> 8  MULTIPOLYGON (((-2.12362 37...
#> 9  MULTIPOLYGON (((-2.270543 3...
#> 10 MULTIPOLYGON (((-2.251954 3...
```
