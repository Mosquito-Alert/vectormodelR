# Attach H3 identifiers to model inputs

Loads an existing H3 grid, or builds it when it does not exist, and
assigns each observation to an H3 cell using its longitude and latitude.

## Usage

``` r
add_h3_grid(
  dataset,
  iso3,
  admin_level,
  admin_name,
  grid_dir = "data/proc",
  resolution = 9L,
  verbose = TRUE,
  write_output = TRUE
)
```

## Arguments

- dataset:

  Either an in-memory model-preparation dataset or a path to the
  corresponding RDS file.

- iso3:

  Three-letter ISO3 country code.

- admin_level:

  Administrative level used when generating the H3 grid.

- admin_name:

  Administrative unit name.

- grid_dir:

  Directory containing H3 grids.

- resolution:

  Integer H3 resolution from 0 to 15.

- verbose:

  Logical. Print progress messages.

- write_output:

  Logical. Write the enriched dataset to disk.

## Value

The dataset with an `h3_id_<resolution>` column.

## Details

Multiple resolutions can be added to the same dataset. Each resolution
is stored in a separate column, such as `h3_id_6` or `h3_id_7`.
