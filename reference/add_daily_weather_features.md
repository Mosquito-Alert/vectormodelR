# Add daily ERA5 weather features to model inputs

Matches observations to the nearest ERA5 cell and adds every column from
the daily weather and precipitation-lag tables.

## Usage

``` r
add_daily_weather_features(
  dataset,
  dataset_type,
  data_dir = "data/proc",
  write_output = TRUE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Model-preparation data or a path to its RDS file.

- dataset_type:

  Either `"reanalysis-era5-land"` or `"reanalysis-era5-single-levels"`.

- data_dir:

  Directory containing processed weather files.

- write_output:

  Whether to write the enriched dataset.

- verbose:

  Whether to print progress messages.

## Value

The model-preparation dataset with daily weather features added.
