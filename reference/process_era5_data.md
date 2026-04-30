# Build daily & lagged weather features from processed ERA5 CSVs

Reads monthly CSV.GZ files created by compile_era5_data_v2 from the
appropriate dataset-specific subdirectory, clips by a GADM admin
polygon, aggregates to hourly area means, derives daily summaries and
rolling-window features, and saves RDS outputs with informative names.

## Usage

``` r
process_era5_data(
  iso3,
  admin_level,
  admin_name,
  dataset,
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

- iso3:

  Character. ISO3 code for the country (e.g., "ESP").

- admin_level:

  Integer. GADM administrative level (0=country, 1=region, 2=province,
  ...).

- admin_name:

  Character or NULL. Exact `NAME_<level>` to match (e.g., "Barcelona").
  If NULL, the whole level geometry is used (unioned).

- dataset:

  Character. ERA5 dataset: "reanalysis-era5-single-levels" or
  "reanalysis-era5-land". Required. Determines which processed folder to
  read from (processed/single-levels/ or processed/land/).

- out_dir:

  Character. Directory to write RDS outputs. Created if missing.

- start_date:

  Date. Earliest date to include (UTC). Default NULL, inferred from
  data.

- end_date:

  Date. Latest date to include (UTC). Default NULL, inferred from data.

- wind_calm_kmh:

  Numeric. Calm-wind threshold in km/h for MWI logic. Default 6.

- round_ll:

  Integer. Rounding decimal places applied to lon/lat before reshaping
  to wide.

- verbose:

  Logical. If TRUE, prints progress messages. Default TRUE.

- attach_to_global:

  Logical. If TRUE, assigns output data.frames to .GlobalEnv.

- aggregation_unit:

  Character. Choose "region", "cell", or "hourly".

- polygon_buffer_km:

  Numeric. If no ERA5 centroids fall inside the admin polygon, or too
  few are captured, expand it by this distance in kilometers.

## Value

For `aggregation_unit = "region"` or `"cell"`, invisibly returns a list
with: `daily`, `lags_7d`, `lags_14d`, `lags_30d`, `lags_21d_lag7`,
`ppt_lags`, and `paths`. For `aggregation_unit = "hourly"`, returns a
list with `hourly` and `paths`.

## Details

ERA5-Land columns are renamed after `dcast()` to match
`era5_single_level`, so downstream calculations use one standard
variable schema.

Temperature and dewpoint values are treated as Kelvin and converted to
Celsius with `x - 273.15`. This is intentional: in some GRIBs, `terra`
may label the layers as `[C]`, but the values can still be Kelvin, e.g.
272–295.

Requires helper objects:

- `era5_single_level`

- `era5_land_to_single_level`

## Examples

``` r
if (FALSE) { # \dontrun{
result <- process_era5_data(
  iso3 = "ESP",
  admin_level = 2,
  admin_name = "Barcelona",
  dataset = "reanalysis-era5-land",
  aggregation_unit = "region"
)

result <- process_era5_data(
  iso3 = "ITA",
  admin_level = 0,
  admin_name = NULL,
  dataset = "reanalysis-era5-single-levels",
  start_date = as.Date("2020-01-01"),
  end_date = as.Date("2023-12-31")
)
} # }
```
