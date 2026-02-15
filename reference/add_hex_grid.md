# Attach hex-grid identifiers to Mosquito Alert model inputs

Reads the precomputed hex grid for a given location, assigns each report
to its containing cell, and writes a new dataset with size-specific grid
identifiers. When the input is provided as an in-memory object it must
carry an `output_path` attribute so the new filename can be derived.

## Usage

``` r
add_hex_grid(
  dataset,
  iso3,
  admin_level,
  admin_name,
  grid_dir = "data/proc",
  cellsize_m = 400,
  verbose = TRUE,
  write_output = TRUE
)
```

## Arguments

- dataset:

  Either the in-memory model-preparation dataset or a path to the
  corresponding RDS file.

- iso3:

  Three-letter ISO3 country code.

- admin_level:

  Administrative level used when generating the hex grid.

- admin_name:

  Administrative unit name used in the file naming scheme.

- grid_dir:

  Directory containing the precomputed hex grids. Defaults to
  "data/proc". The function searches for files following the
  `spatial_<slug>_hex_grid_<cellsize>.rds` naming convention, with
  fallbacks to legacy filenames when necessary.

- cellsize_m:

  Numeric cell size (meters) used when the grid was generated. Defaults
  to 400 to match the package's standard grid.

- verbose:

  Logical; if `TRUE`, prints status updates. Defaults to `TRUE`.

- write_output:

  Logical flag; when `TRUE` (default) the enriched dataset is written to
  disk with `_hex<cellsize>.Rds` appended to the filename stem (for
  example `_hex400.Rds`).

## Value

A tibble/data frame containing the augmented dataset. Attributes from
the input object are preserved and supplemented with `hex_grid_source`,
`location_slug`, and an updated `output_path`. Each call adds a
size-specific `grid_id_<cellsize>` column, and the `hex_grid_id_column`
attribute records the available grid identifiers.
