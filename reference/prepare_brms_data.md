# Prepare data for brms modelling

Cleans the dataset, scales predictors (storing means/SDs for future
use), and returns an object ready to be passed to
[`run_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_model.md).
Optionally saves the prepared object to disk.

## Usage

``` r
prepare_brms_data(
  dataset,
  cellsize_m = 800,
  temporal_resolution = c("daily", "hourly"),
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  output_dir = "data/proc",
  write = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  An in-memory modelling dataset (data.frame), a `brms_data_prep`
  object, a `bym2_data_prep` object, or a path to a prepared RDS file.

- cellsize_m:

  Numeric cell size (meters). Defaults to 800.

- temporal_resolution:

  Character. Either `"daily"` (default) or `"hourly"`. When `"hourly"`,
  filters to records with valid hourly weather values and scales the
  current-hour and short-window hourly predictors. When `"daily"`, uses
  the daily weather summaries and precipitation lag predictors.

- iso3, admin_level, admin_name:

  Optional strings to locate the dataset if `dataset` is NULL.

- output_dir:

  Directory where the prepared dataset is written when `write = TRUE`.
  Defaults to `"data/proc"`.

- write:

  Logical. If `TRUE` the prepared object is written to disk. Defaults to
  `FALSE`.

- verbose:

  Logical. Emit informative messages when `TRUE`.

## Value

A list of class `brms_data_prep` containing:

- model_data:

  The filtered, scaled data.frame ready for brms.

- grid_col:

  The name of the grid identifier column used.

- scaling:

  A list of mean/sd values used for scaling predictors.

- meta:

  Metadata (iso3, admin_level, etc.) for file naming.
