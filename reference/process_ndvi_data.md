# Process NDVI raster by clipping to an administrative boundary

Loads an NDVI raster (e.g., a Sentinel-2 composite), aligns it with a
target boundary, crops and masks pixels outside the polygon, writes the
masked output if requested, and computes simple global summary
statistics.

## Usage

``` r
process_ndvi_data(
  ndvi,
  iso3,
  admin_level,
  admin_name,
  write_raster = TRUE,
  write_stats = TRUE,
  proc_dir = "data/proc",
  datatype = "FLT4S",
  verbose = TRUE
)
```

## Arguments

- ndvi:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or path to an NDVI GeoTIFF file.

- iso3:

  Three-letter ISO3 country code used to locate the boundary file.

- admin_level:

  Administrative level associated with the boundary.

- admin_name:

  Administrative unit name used in the file naming scheme.

- write_raster:

  Logical. If `TRUE`, save the masked raster via
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

- write_stats:

  Logical. If `TRUE`, persist the summary statistics to disk (RDS).

- proc_dir:

  Directory used for any outputs. Created when missing.

- datatype:

  GDAL datatype passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).
  Defaults to `"FLT4S"` for floating-point NDVI values.

- verbose:

  Logical. If `TRUE`, prints progress messages.

## Value

A list with the masked NDVI raster (`raster`) and a tibble of summary
statistics (`stats`).
