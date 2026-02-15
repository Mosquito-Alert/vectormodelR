# Generate pseudoabsences using TRS effort (Mosquito Alert) and TGB (GBIF)

Samples pseudoabsences separately for Mosquito Alert (TRS-based effort
surface) and GBIF (target-group background weights). The function
expects the presence dataset to include coordinates (configurable via
`lon_col`/`lat_col`), the specified date column, and a source column
distinguishing Mosquito Alert vs GBIF records. When supplied an
in-memory dataset it must carry an `output_path` attribute so the helper
can infer the location slug and persist the augmented output.

## Usage

``` r
add_pseudoabsences_se(
  dataset,
  data_dir = "data/proc",
  sampling_factor_ma = 10,
  sampling_factor_gbif = 10,
  date_col = "date",
  lon_col = "lon",
  lat_col = "lat",
  source_col = "source",
  se_col = "SE_expected",
  tgb_col = "tgb_w",
  ma_source = "malert",
  gbif_source = "gbif",
  write_output = TRUE
)
```

## Arguments

- dataset:

  Either the in-memory modelling dataset or a path to the corresponding
  RDS file.

- data_dir:

  Directory holding processed datasets (TRS/TGB artefacts) and where the
  pseudoabsence dataset will be written. Defaults to "data/proc".

- sampling_factor_ma:

  Mosquito Alert pseudoabsences per MA presence (default 10).

- sampling_factor_gbif:

  GBIF pseudoabsences per GBIF presence (default 10).

- date_col:

  Name of the date column shared by the presence and effort tables
  (default `"date"`).

- lon_col:

  Name of longitude column in the presence dataset (default `"lon"`).

- lat_col:

  Name of latitude column in the presence dataset (default `"lat"`).

- source_col:

  Name of the data-source column in the presence dataset (default
  `"source"`).

- se_col:

  Name of the TRS sampling-effort column (default `"SE_expected"`).

- tgb_col:

  Name of the GBIF target-group background weight column (default
  `"tgb_w"`).

- ma_source:

  Label used for Mosquito Alert records in `source_col` (default
  `"malert"`).

- gbif_source:

  Label used for GBIF records in `source_col` (default `"gbif"`).

- write_output:

  Logical; when `TRUE` (default) persists the combined
  presence+pseudoabsence dataset to disk.

## Value

A tibble combining presences (`presence = TRUE`) and pseudoabsences
(`presence = FALSE`) with numeric coordinate columns
(`lon_col`/`lat_col`) plus `pa_method` metadata. Attributes mirror the
input dataset with updated `output_path` and `location_slug` values.
