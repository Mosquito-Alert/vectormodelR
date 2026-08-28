# Build an H3 grid for a supplied polygon

Creates an H3 grid at a specified resolution and returns the cells as an
`sf` object. Cells are selected when their centres fall inside the
supplied polygon.

## Usage

``` r
build_h3_grid(
  map = NULL,
  resolution = 9L,
  clip = FALSE,
  return_crs = 4326,
  write = TRUE,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  data_dir = "data/proc",
  verbose = TRUE
)
```

## Arguments

- map:

  Optional `sf` object describing the polygon(s) to cover.

- resolution:

  Integer H3 resolution from 0 to 15. Defaults to 9.

- clip:

  Logical. If `TRUE`, clips cells to the polygon boundary. Defaults to
  `FALSE`.

- return_crs:

  Coordinate reference system for the returned grid. Defaults to
  EPSG:4326.

- write:

  Logical. If `TRUE`, writes the grid to `data_dir`.

- iso3:

  Optional ISO3 country code used with `admin_level` and `admin_name`
  when `map` is not supplied.

- admin_level:

  Optional administrative level.

- admin_name:

  Optional administrative unit name.

- data_dir:

  Directory containing boundary inputs and grid outputs.

- verbose:

  Logical. If `TRUE`, reports progress and file paths.

## Value

An `sf` object containing `h3_id`, `grid_id`, `h3_id_<resolution>`,
`h3_resolution`, and polygon geometry.

## Details

H3 uses integer resolutions from 0 to 15 rather than exact cell sizes in
metres. By default, complete H3 geometries are retained so their
identifiers, boundaries, and neighbour relationships remain valid.

## Examples

``` r
if (FALSE) { # \dontrun{
h3_grid <- build_h3_grid(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona",
  resolution = 9,
  write = TRUE
)

h3_grid <- build_h3_grid(
  map = bcn_poly,
  resolution = 9,
  clip = FALSE,
  write = FALSE
)
} # }
```
