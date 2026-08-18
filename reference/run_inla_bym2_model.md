# Fit a BYM2 model with INLA

Fits a complete user-supplied INLA formula using data prepared by
[`prepare_inla_bym2_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_inla_bym2_data.md).
The formula must include the BYM2 spatial term.

## Usage

``` r
run_inla_bym2_model(
  dataset = NULL,
  formula,
  family = "binomial",
  Ntrials = NULL,
  bym2_hyper = NULL,
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

  An `inla_bym2_data_prep` object, a path to a saved preparation object,
  or `NULL`.

- formula:

  Complete INLA formula, including the BYM2 spatial term.

- family:

  INLA likelihood family. Defaults to `"binomial"`.

- Ntrials:

  Optional binomial trial counts.

- bym2_hyper:

  Optional BYM2 hyperprior specification available inside the formula as
  `bym2_hyper`.

- temporal_resolution:

  Either `"daily"` or `"hourly"`.

- iso3, admin_level, admin_name:

  Location identifiers used when `dataset = NULL`.

- input_dir:

  Directory containing prepared INLA BYM2 data.

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

## Details

The prepared spatial graph is available inside the formula as
`spatial_graph`. When the formula contains `space_time_id`, the prepared
Knorr-Held Type IV objects are made available as `R_int`, `A_kh`, and
`e_kh`.
