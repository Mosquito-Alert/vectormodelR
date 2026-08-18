# Fit a general model with INLA

Fits a user-supplied INLA formula using data prepared by
[`prepare_inla_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_inla_data.md).

## Usage

``` r
run_inla_model(
  dataset = NULL,
  formula,
  family = "binomial",
  Ntrials = NULL,
  temporal_resolution = c("daily", "hourly"),
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  input_dir = "data/proc",
  control.family = list(link = "logit"),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  control.predictor = list(compute = TRUE, link = 1),
  inla_args = list(),
  write_output = TRUE,
  output_path = "data/proc",
  verbose = TRUE
)
```

## Arguments

- dataset:

  An `inla_data_prep` object, a path to a saved preparation object, or
  `NULL`.

- formula:

  A formula or single character string accepted by `INLA::inla()`.

- family:

  INLA likelihood family. Defaults to `"binomial"`.

- Ntrials:

  Optional binomial trial counts. When `NULL`, one trial per observation
  is used.

- temporal_resolution:

  Either `"daily"` or `"hourly"`.

- iso3, admin_level, admin_name:

  Location identifiers used when `dataset = NULL`.

- input_dir:

  Directory containing prepared INLA data.

- control.family:

  List passed to `INLA::inla(control.family = ...)`.

- control.compute:

  List passed to `INLA::inla(control.compute = ...)`.

- control.predictor:

  List passed to `INLA::inla(control.predictor = ...)`.

- inla_args:

  Additional named arguments passed to `INLA::inla()`.

- write_output:

  Whether to save the fitted model.

- output_path:

  Output directory or RDS filename.

- verbose:

  Whether to emit progress messages.

## Value

A fitted `inla` object.

## Examples

``` r
if (FALSE) { # \dontrun{
inla_data <- prepare_inla_data(
  dataset = brms_dataset_daily,
  landcover_reference = "Built-up",
  temperature_groups = 30
)

occupancy_formula <- presence ~
  f(
    sea_day_id,
    model = "rw2",
    cyclic = TRUE,
    values = 1:365,
    constr = TRUE,
    scale.model = TRUE
  ) +
  f(
    maxTM_group,
    model = "rw2",
    constr = TRUE,
    scale.model = TRUE
  ) +
  ppt_3d_lag7_z +
  ndvi_z +
  elev_z +
  pop_z +
  landcover_class +
  source +
  f(
    year_id,
    model = "iid",
    constr = TRUE
  )

fit <- run_inla_model(
  dataset = inla_data,
  formula = occupancy_formula
)
} # }
```
