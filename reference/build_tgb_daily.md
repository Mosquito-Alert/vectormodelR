# Build GBIF target-group background daily weights on the TRS/Tigacell geometry

Creates `model_prep_<iso3>_<admin_level>_<admin_name>_tgb_daily.Rds` by
matching GBIF occurrences to the nearest TRS/Tigacell point and
aggregating daily counts.

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
  max_dist_m = NULL,
  write_output = TRUE,
  overwrite = FALSE
)
```

## Arguments

- iso3:

  Three-letter ISO3 country code used in artefact names.

- admin_level:

  Administrative level identifier used in artefact names.

- admin_name:

  Administrative unit name used in artefact names.

- vector_dir:

  Directory containing `vector_<slug>_gbif.Rds`. Defaults to
  "data/proc".

- data_dir:

  Directory containing `model_prep_<slug>_trs_daily.Rds` and where the
  TGB output should be written. Defaults to "data/proc".

- weight_col:

  Name of the aggregated weight column. Defaults to "tgb_w".

- time_bin:

  Either "day" (default) or "year"; controls how occurrence timestamps
  are binned.

- max_dist_m:

  Optional maximum distance threshold (in meters) for nearest-neighbour
  assignments. Occurrences farther than this distance from their nearest
  TRS point are dropped.

- write_output:

  Logical; write the output RDS when TRUE (default).

- overwrite:

  Logical; if FALSE (default) and the output already exists, an error is
  raised.

## Value

An `sf` object with columns `date`, `year`, and the chosen weight
column. Attributes include `output_path`, `gbif_source`, and
`trs_source`.
