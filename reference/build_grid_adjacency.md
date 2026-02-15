# Build adjacency matrix for a grid (global-safe)

Reads a polygon grid (`spatial_*_hex_grid_<cellsize>.Rds`) and prepared
model data (`model_prep_*_wx_lc_ndvi_elev.Rds`), keeps only cells
present in model data, reprojects (only if needed) to a locally
appropriate projected CRS, and returns a binary queen-contiguity
adjacency matrix suitable for `brms::car()`. The function first searches
for size-specific grid filenames and falls back to legacy names without
the `<cellsize>` token.

## Usage

``` r
build_grid_adjacency(
  iso3,
  admin_level,
  admin_name,
  data_dir = "data/proc",
  cellsize_m = 400,
  target_crs = NULL,
  sparse = TRUE,
  model
)
```

## Arguments

- iso3:

  Three-letter ISO3 country code used in the slug.

- admin_level:

  Administrative level tied to the boundary/grid slug.

- admin_name:

  Administrative unit name used in the file naming scheme.

- data_dir:

  Directory holding processed outputs. Defaults to `"data/proc"`.

- cellsize_m:

  Numeric cell size (meters) for the hex grid file name. Defaults
  to 400. Use values matching the
  [`build_spatial_grid()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_spatial_grid.md)
  output (for example, setting `cellsize_m = 800` targets
  `spatial_*_hex_grid_800.Rds` and the corresponding `grid_id_800`
  column).

- target_crs:

  Optional. EPSG code or
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  object. If `NULL`, a suitable CRS is chosen automatically (UTM if
  input is lon/lat).

- sparse:

  Logical. If TRUE (default), return a sparse Matrix.

- model:

  Either the in-memory model dataset or a path to a prepared
  `model_prep_*` RDS file. This must be supplied; the function no longer
  derives the model path automatically.

## Value

An adjacency matrix (dense base matrix if `sparse = FALSE`, otherwise a
`Matrix::dgCMatrix`) with row/col names equal to the size-specific grid
identifier (for example `grid_id_400`).
