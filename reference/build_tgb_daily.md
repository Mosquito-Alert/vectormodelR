# Build GBIF target-group background data at original locations

Loads GBIF target-group records, retains their original coordinates, and
aggregates duplicate records by location and time period. Spatial grid
IDs are assigned later by the model preparation function.

## Usage

``` r
build_tgb_daily(
  iso3,
  admin_level,
  admin_name,
  vector_dir = "data/proc",
  data_dir = "data/proc",
  weight_col = "tgb_w",
  time_bin = c("day", "year"),
  write_output = TRUE,
  overwrite = FALSE
)
```

## Arguments

- iso3:

  Three-letter ISO3 country code.

- admin_level:

  Administrative level identifier.

- admin_name:

  Administrative unit name.

- vector_dir:

  Directory containing `vector_<slug>_gbif.Rds`.

- data_dir:

  Directory where the output should be written.

- weight_col:

  Name of the target-group weight column.

- time_bin:

  Either `"day"` or `"year"`.

- write_output:

  Whether to write the output RDS file.

- overwrite:

  Whether to replace an existing output file.

## Value

An `sf` point object containing `lon`, `lat`, `date`, `year`, and the
target-group weight column.
