# Process land-cover raster by clipping to a GADM boundary

This helper loads a land-cover raster (e.g., ESA WorldCover GeoTIFF),
crops it to the extent of a GADM administrative boundary, masks pixels
outside the boundary, attaches an ESA WorldCover attribute table, and
returns the masked
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
Optionally, the masked layer can be written to disk via
[`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

## Usage

``` r
process_landcover_data(
  landcover,
  iso3,
  admin_level,
  admin_name,
  write_raster = TRUE,
  proc_dir = "data/proc",
  datatype = "INT1U",
  verbose = TRUE
)
```

## Arguments

- landcover:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or path to a land-cover raster file.

- iso3:

  Three-letter ISO3 country code used to locate the boundary file.

- admin_level:

  Administrative level associated with the boundary.

- admin_name:

  Administrative unit name used in the file naming scheme.

- write_raster:

  Logical. If `TRUE`, write the masked raster using
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

- proc_dir:

  Directory used when `write_raster = TRUE`. Defaults to `"data/proc"`.

- datatype:

  GDAL datatype passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).
  Defaults to `"INT1U"`, which preserves the categorical land-cover
  codes.

- verbose:

  Logical. If `TRUE`, prints progress messages.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
representing the cropped and masked land-cover layer with ESA WorldCover
class metadata attached via
[`levels()`](https://rspatial.github.io/terra/reference/factors.html).
