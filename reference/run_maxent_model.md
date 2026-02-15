# Fit a baseline Mosquito Alert MaxEnt model (maxnet)

Reads a Mosquito Alert modelling dataset (from disk or memory), prepares
predictors, and fits a MaxEnt-style model using maxnet. MaxEnt requires
presence/background data (1/0). If the dataset contains only presences,
background points can be sampled from unique grid cells.

## Usage

``` r
run_maxent_model(
  dataset,
  predictors = c("maxTM", "meanPPT24H", "ndvi_ddf_proximity", "elevation_m",
    "popdensity_km2", "landcover_code"),
  presence_col = "presence",
  grid_id_col = "grid_id",
  feature_classes = c("l", "q", "h"),
  regmult = 1,
  n_background = 10000,
  seed = 1234,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  write_output = TRUE,
  output_path = "data/proc",
  verbose = TRUE
)
```

## Arguments

- dataset:

  A data frame or a path to an .Rds containing one.

- predictors:

  Character vector of predictor column names to use. Defaults to a
  baseline set (no sea_days).

- presence_col:

  Name of presence column. Defaults to "presence". Expected to be
  logical TRUE/FALSE, or 0/1.

- grid_id_col:

  Optional column used to sample background by unique grid cells
  (defaults to "grid_id"). If missing, background is sampled by rows.

- feature_classes:

  Maxnet feature classes (e.g. c("l","q","h")).

- regmult:

  Regularization multiplier passed to maxnet.

- n_background:

  Number of background samples if presences-only.

- seed:

  Optional seed for reproducibility.

- iso3, admin_level, admin_name:

  Optional naming inputs used to derive a slug via
  build_location_identifiers(). Supply all three together when used.

- write_output:

  Save fitted model to disk? Defaults TRUE.

- output_path:

  Directory or full file path for saving. Defaults "data/proc".

- verbose:

  Print progress messages? Defaults TRUE.

## Value

A fitted maxnet model object. Attributes:

- "model_data" (data used to fit)

- "output_path" (if saved)

- "source_dataset" (if dataset was a path)

- "location_slug" (if available)
