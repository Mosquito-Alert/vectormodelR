# Add hourly ERA5 weather features to model inputs

Matches each observation to its nearest ERA5 cell and report hour, then
joins all available hourly weather columns.

## Usage

``` r
add_hourly_weather_features(
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

  ERA5 dataset type.

- data_dir:

  Directory containing processed weather files.

- write_output:

  Whether to save the enriched dataset.

- verbose:

  Whether to print progress messages.

## Value

The dataset with hourly weather features added.
