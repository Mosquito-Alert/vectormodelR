# Process population raster by clipping to a GADM boundary and computing density

Loads a population raster (e.g., WorldPop ppp), crops it to a stored
boundary, masks outside pixels, converts to population density (people /
km^2), and optionally writes a GeoTIFF to disk.

## Usage

``` r
process_popdensity_data(
  population,
  iso3,
  admin_level,
  admin_name,
  write_raster = TRUE,
  proc_dir = "data/proc",
  datatype = "FLT4S",
  verbose = TRUE
)
```

## Arguments

- population:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or path to a population GeoTIFF. If this is WorldPop ppp (people per
  pixel), density is computed as ppp / cell_area_km2.

- iso3:

  Three-letter ISO3 country code used to locate the boundary file.

- admin_level:

  Administrative level associated with the boundary.

- admin_name:

  Administrative unit name used in the file naming scheme.

- write_raster:

  Logical. If `TRUE`, write the masked density raster using
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

- proc_dir:

  Directory used when `write_raster = TRUE`. Defaults to `"data/proc"`.

- datatype:

  GDAL datatype passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).
  Defaults to `"FLT4S"`.

- verbose:

  Logical. If `TRUE`, prints progress messages.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
of population density (people / km^2) clipped to boundary.
