# Add ERA5 weather features to Mosquito Alert model inputs

Starts from the model-preparation dataset produced by
[`initialize_ma_dataset()`](https://labs.mosquitoalert.com/mosquitoR/reference/initialize_ma_dataset.md),
attaches ERA5-based weather metrics, and writes a new
`model_prep_*_wx.Rds` file alongside the existing outputs.

## Usage

``` r
add_weather_features(
  dataset,
  data_dir = "data/proc",
  write_output = TRUE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Either the in-memory base model-prep dataset (as returned by
  [`initialize_ma_dataset()`](https://labs.mosquitoalert.com/mosquitoR/reference/initialize_ma_dataset.md)
  or subsequent enrichment helpers) or a path to the corresponding RDS
  file. When a data object is supplied it must carry an `output_path`
  attribute naming the last saved file; the enriched dataset is written
  to `data_dir` with `_wx.Rds` appended to that stem when `write_output`
  is `TRUE`.

- data_dir:

  Directory that holds the weather RDS files and where the
  weather-enriched dataset will be written. Defaults to `"data/proc"`.

- write_output:

  Logical flag; when `TRUE` (default) the enriched dataset is written to
  disk. Set to `FALSE` to skip writing while still returning the
  augmented object and updating its metadata.

- verbose:

  Logical; if `TRUE`, prints status messages.

## Value

A tibble containing the enriched dataset. Attributes from the base
dataset are preserved, and additional attributes (`weather_sources` and
`output_path`) identify the files that were read and written.

## Examples

``` r
if (FALSE) { # \dontrun{
add_weather_features(
  dataset = initialize_ma_dataset(
    iso3 = "ESP",
    admin_level = 4,
    admin_name = "Barcelona"
  )
)
} # }
```
