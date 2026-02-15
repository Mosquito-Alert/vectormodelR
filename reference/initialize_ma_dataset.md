# Prepare Mosquito Alert reports for modelling

Prepare Mosquito Alert reports for modelling

## Usage

``` r
initialize_ma_dataset(
  iso3,
  admin_level,
  admin_name,
  reports_path = "data/proc/mosquito_alert_cleaned_reports.Rds",
  sampling_effort_url =
    "https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz",
  android_start_date = NULL,
  output_dir = "data/proc",
  dataset_variant = "base"
)
```

## Arguments

- iso3:

  Character scalar with the three-letter ISO3 country code.

- admin_level:

  Administrative level used when the hex grid was created. Accepts
  character or numeric input.

- admin_name:

  Name of the administrative unit used when the hex grid was created.
  The name is normalised (lowercase, non-alphanumerics replaced with
  underscores) before constructing the file path.

- reports_path:

  Path to the cleaned Mosquito Alert reports RDS file.

- sampling_effort_url:

  Optional URL pointing to the sampling effort CSV resource. Set to
  `NULL` to skip downloading.

- android_start_date:

  Optional date (character or `Date`) used to filter reports collected
  before the Android application launch.

- output_dir:

  Directory where the prepared dataset should be written and where
  supporting boundary artefacts are expected. Defaults to `"data/proc"`.

- dataset_variant:

  Optional suffix inserted into the saved filename (for example "base"
  or "extended").

## Value

A tibble of filtered reports. The object includes attributes such as
`sampling_effort` and `output_path` for downstream reuse.

## Examples

``` r
if (FALSE) { # \dontrun{
initialize_ma_dataset(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona"
)
} # }
```
