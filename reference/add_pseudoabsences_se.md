# Generate pseudoabsences using TRS effort and TGB weights

Samples pseudoabsences separately for Mosquito Alert and GBIF. Before
sampling, the requested model grid is added to each effort dataset and
candidates overlapping known presences are removed.

## Usage

``` r
add_pseudoabsences_se(
  dataset,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  data_dir = "data/proc",
  temporal_resolution = c("daily", "hourly"),
  sampling_factor_ma = 10,
  sampling_factor_gbif = 10,
  cell_id_col = "cell_id",
  date_col = "date",
  hour_col = "hour",
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

  In-memory modelling dataset or path to an RDS file.

- iso3:

  Three-letter country code.

- admin_level:

  Administrative level.

- admin_name:

  Administrative-area name.

- data_dir:

  Directory containing processed datasets.

- temporal_resolution:

  Either `"daily"` or `"hourly"`.

- sampling_factor_ma:

  Pseudoabsences per Mosquito Alert presence.

- sampling_factor_gbif:

  Pseudoabsences per GBIF presence.

- cell_id_col:

  Spatial-cell column, such as `h3_id_9` or `hex_id_1200`.

- date_col:

  Date column.

- hour_col:

  Hour column.

- lon_col:

  Longitude column.

- lat_col:

  Latitude column.

- source_col:

  Data-source column.

- se_col:

  TRS sampling-effort weight column.

- tgb_col:

  GBIF target-group background weight column.

- ma_source:

  Mosquito Alert source label.

- gbif_source:

  GBIF source label.

- write_output:

  Whether to save the result.

## Value

A tibble containing known presences and generated pseudoabsences.
