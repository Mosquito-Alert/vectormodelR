# Build a regular grid for a supplied polygon

Creates a tessellation (hexagonal or square) at a given cell size
(meters), optionally clips the grid to the polygon footprint, and
returns the grid in the desired CRS. Uses
[`convert_to_utm()`](https://labs.mosquitoalert.com/mosquitoR/reference/convert_to_utm.md)
to pick an appropriate metric projection.

## Usage

``` r
build_spatial_grid(
  map = NULL,
  cellsize_m = 400,
  clip = TRUE,
  return_crs = 4326,
  shape = c("hex", "square"),
  write = TRUE,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  data_dir = "data/proc"
)
```

## Arguments

- map:

  Optional `sf` object describing the polygon(s) to cover.

- cellsize_m:

  Numeric. Grid cell size in meters (default 400).

- clip:

  Logical. If TRUE (default), intersect hexes with the polygon’s
  outline.

- return_crs:

  Coordinate reference system for the output (default 4326).

- shape:

  Character. "hex" (default) for hexagons or "square" for squares.

- write:

  Logical. If TRUE, saves the generated grid to `data_dir` using the
  slug-based filename (for example `spatial_<slug>_hex_grid_400.Rds`).

- iso3:

  Optional three-letter ISO3 code used with `admin_level` and
  `admin_name` to locate boundary files when `map` is not supplied.

- admin_level:

  Optional administrative level corresponding to the boundary file when
  `map` is not supplied.

- admin_name:

  Optional administrative unit name corresponding to the boundary file
  when `map` is not supplied.

- data_dir:

  Directory where boundary inputs and grid outputs are stored. Defaults
  to `"data/proc"`.

## Value

An `sf` object with polygons and a `grid_id` column. A size-specific
`grid_id_<cellsize>` column is also added to support storing multiple
grid resolutions side-by-side.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Example polygon (Barcelona bbox)
bcn_poly <- st_as_sfc(st_bbox(c(
  xmin = 2.05, xmax = 2.25,
  ymin = 41.30, ymax = 41.50
)), crs = 4326)

# Hexagonal grid at 400 m cells, returning sf object
hex_grid <- build_spatial_grid(bcn_poly, cellsize_m = 400)

# Square grid at 1 km cells, saving to disk
sq_grid <- build_spatial_grid(
  map = bcn_poly,
  cellsize_m = 1000,
  shape = "square",
  write = FALSE
)

# Using stored administrative boundaries (requires matching RDS file)
hex_grid <- build_spatial_grid(
  iso3 = "ESP",
  admin_level = 2,
  admin_name = "Barcelona",
  write = TRUE
)
} # }
```
