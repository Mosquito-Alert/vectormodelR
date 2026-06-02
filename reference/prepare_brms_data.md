# Prepare data for brms modelling

Cleans the dataset, optionally filters rows with missing values in
user-supplied variables, aggregates repeated records to modelling units,
optionally converts selected columns to factors, scales predictors using
user-supplied scaling specifications, and returns an object ready to be
passed to
[`run_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_model.md).
Optionally saves the prepared object to disk.

## Usage

``` r
prepare_brms_data(
  dataset,
  cellsize_m = 800,
  temporal_resolution = c("daily", "hourly"),
  base_required_cols = NULL,
  vars_to_check = NULL,
  scale_specs = NULL,
  aggregation_specs = NULL,
  factor_cols = NULL,
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

  Numeric cell size in meters. Defaults to 800.

- temporal_resolution:

  Character. Either `"daily"` or `"hourly"`. When `"daily"`, records are
  aggregated to source-grid-date units. When `"hourly"`, records are
  aggregated to source-grid-date-hour units.

- base_required_cols:

  Optional character vector of column names that must exist for the
  preparation machinery to run. The active grid column is always added
  automatically. If `NULL`, defaults to
  `c("date", "presence", "source")`.

- vars_to_check:

  Optional character vector of column names to check for missing values
  before aggregation and scaling. If `NULL`, no explicit missing-value
  filtering is applied beyond required grid/hour checks.

- scale_specs:

  Optional named list of scaling specifications. If `NULL`, no predictor
  scaling is applied. Specs may be plain named lists with fields
  `input`, `output`, optional `transform`, and optional `scale_name`;
  these are normalized internally by `normalize_scale_specs()`.

- aggregation_specs:

  Optional named list or named character vector defining how columns
  should be aggregated when multiple rows occur in the same modelling
  unit. Names are column names and values are aggregation methods.
  Supported methods are `"any"`, `"all"`, `"mean"`, `"median"`, `"sum"`,
  `"min"`, `"max"`, `"first"`, `"last"`, `"mode"`, and `"paste_unique"`.
  If `NULL`, all non-grouping columns are aggregated using `"first"`,
  except `presence`, which defaults to `"any"`.

- factor_cols:

  Optional character vector of columns to convert to factors after
  aggregation and before scaling/modelling. If `NULL`, no columns are
  converted. For typical models, use columns such as `"year"`,
  `"landcover_code"`, and `"source"` when they should be treated as
  categorical predictors or grouping variables.

- iso3, admin_level, admin_name:

  Optional strings to locate the dataset if `dataset` is NULL.

- output_dir:

  Directory where the prepared dataset is written when `write = TRUE`.
  Defaults to `"data/proc"`.

- write:

  Logical. If `TRUE`, the prepared object is written to disk. Defaults
  to `FALSE`.

- verbose:

  Logical. Emit informative messages when `TRUE`.

## Value

A list of class `brms_data_prep` containing:

- model_data:

  The filtered, aggregated, scaled data.frame ready for brms.

- grid_col:

  The name of the grid identifier column used.

- scaling:

  A list of mean/sd values used for scaling predictors.

- scale_specs:

  The normalized scaling specifications supplied to the function.

- aggregation_specs:

  The aggregation specifications used.

- factor_cols:

  The factor columns used.

- meta:

  Metadata for file naming and reproducibility.
