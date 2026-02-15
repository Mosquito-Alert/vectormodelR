# Get ~30 m SRTM DEM for a GADM admin unit or full country

Downloads a ~30 m (1 arc-second) DEM via elevatr for a selected
administrative unit (or entire country) from geodata and returns a terra
SpatRaster.

## Usage

``` r
get_elevation_data(
  country,
  level = 2,
  name_value = NULL,
  z = 11,
  path = "data/gadm",
  write_tif = TRUE,
  proc_dir = "data/proc"
)
```

## Arguments

- country:

  Country name or ISO3 code accepted by
  [`geodata::gadm()`](https://rdrr.io/pkg/geodata/man/gadm.html).

- level:

  Integer GADM level (0 = country, 1 = region, 2 = district/province,
  etc.). Default: 2.

- name_value:

  Character. Name of the unit to select (matched against the relevant
  NAME column). If `NULL`, the full level is used.

- z:

  Integer zoom for
  [`elevatr::get_elev_raster()`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
  (11 ≈ ~30 m). Default: 11.

- path:

  Directory to cache GADM files. Default: "data/".

- write_tif:

  Logical. If TRUE (default), write the DEM to
  `data/proc/spatial_<iso3>_<level>[_name]_elevation.tif` as a GeoTIFF.

- proc_dir:

  Directory to store the DEM when `write_tif = TRUE`. Default:
  "data/proc".

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
DEM clipped to the selected admin unit or country.

## Examples
