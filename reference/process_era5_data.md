# Build daily & lagged weather features from processed ERA5 CSVs

Reads monthly CSV.GZ files created by your ERA5 compiler (one file per
month with multiple variables named like
`era5_<iso3>_YYYY_MM_all_variables.csv.gz`), clips by a GADM admin
polygon, aggregates to hourly area means, derives daily summaries and
rolling-window features, and saves RDS outputs with informative names.

## Usage

``` r
process_era5_data(
  processed_dir = NULL,
  iso3,
  admin_level,
  admin_name,
  out_dir = "data/proc",
  start_date = NULL,
  end_date = NULL,
  wind_calm_kmh = 6,
  round_ll = 3,
  verbose = TRUE,
  attach_to_global = FALSE,
  aggregation_unit = c("region", "cell", "hourly"),
  polygon_buffer_km = 10
)
```

## Arguments

- processed_dir:

  Character. Root dir containing
  `processed/YYYY/era5_<iso3>_YYYY_MM_all_variables.csv.gz`. If NULL,
  defaults to
  `file.path("data/weather/grib", tolower(iso3), "processed")`.

- iso3:

  Character. ISO3 code for the country (e.g., "ESP").

- admin_level:

  Integer. GADM administrative level (0=country, 1=region, 2=province,
  ...).

- admin_name:

  Character or NULL. Exact `NAME_<level>` to match (e.g., "Barcelona").
  If NULL, the whole level geometry is used (unioned).

- out_dir:

  Character. Directory to write RDS outputs. Created if missing.

- start_date:

  Date. Earliest date to include (UTC). Default "2014-01-01".

- end_date:

  Date. Latest date to include (UTC). Default is today.

- wind_calm_kmh:

  Numeric. Calm-wind threshold in km/h for MWI logic. Default 6.

- round_ll:

  Integer. Rounding (decimal places) applied to lon/lat before reshaping
  to wide. Default 3 (≈100–120 m grid at mid-latitudes).

- verbose:

  Logical. If TRUE, prints progress messages. Default TRUE.

- attach_to_global:

  Logical. If TRUE, assigns output data.frames to .GlobalEnv with names
  based on the file prefix (e.g., `weather_esp_lvl2_barcelona_daily`).
  Default FALSE.

- aggregation_unit:

  Character. Choose "region" (default) to produce polygon-wide means,
  "cell" for per-ERA5-cell summaries, or "hourly" to return the raw
  per-cell hourly series without further aggregation.

- polygon_buffer_km:

  Numeric. If no ERA5 centroids fall inside the admin polygon, expand it
  by this distance (kilometers) and retry. Default 10.

## Value

For `aggregation_unit = "region"` or `"cell"`, (invisibly) a list with:
`daily`, `lags_7d`, `lags_14d`, `lags_30d`, `lags_21d_lag7`, `ppt_lags`,
and `paths` (written file paths). For `aggregation_unit = "hourly"`,
returns a list with `hourly` (raw per-cell series) and `paths`.

## Details

Expects variables in the CSVs:

- "10m_u_component_of_wind", "10m_v_component_of_wind"

- "2m_temperature", "2m_dewpoint_temperature"

- "surface_pressure", "total_precipitation"

Units assumed (ERA5 defaults): temperature in Kelvin, precipitation in
meters per step; wind components in m/s.
