# Build a TIGA-like grid over a GADM admin unit

Build a TIGA-like grid over a GADM admin unit

## Usage

``` r
build_tigacell_grid(
  iso3,
  gadm_level = 0,
  admin_name = NULL,
  cellsize_deg = 0.025,
  path = "data/gadm",
  clip = TRUE,
  id_precision = 3,
  rds = TRUE,
  as_sp = FALSE,
  quiet = FALSE
)
```

## Arguments

- iso3:

  ISO3 country code, e.g. "ESP".

- gadm_level:

  GADM level (0=country, 1=region, 2=province, ...).

- admin_name:

  Optional. Exact NAME\_ to keep (e.g. "Barcelona"). If NULL, uses the
  union of all units at that level.

- cellsize_deg:

  Grid cell size in degrees (lon/lat). Default 0.025.

- path:

  Directory to cache GADM downloads.

- clip:

  If TRUE (default) intersect cells with the polygon; if FALSE, keep
  full bbox grid.

- id_precision:

  Decimal places for the corner coords in TigacellID. Default 3.

- rds:

  Logical. If TRUE (default), write the grid to
  `data/proc/spatial_iso3_level[_admin]_grid.rds`.

- as_sp:

  If TRUE, return a SpatialPolygonsDataFrame (sp). Otherwise sf
  (default).

- quiet:

  Suppress messages.

## Value

An sf (or sp) polygon grid with column `TigacellID`.
