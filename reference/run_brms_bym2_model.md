# Fit a BYM2 Mosquito Alert occupancy model with brms

Fits a Bayesian occupancy model using `brms` with a BYM2 spatial random
effect based on a grid adjacency matrix.

## Usage

``` r
run_brms_bym2_model(
  dataset = NULL,
  formula,
  cellsize = 800,
  temporal_resolution = c("daily", "hourly"),
  adjacency = NULL,
  adjacency_args = list(),
  priors = NULL,
  nchains = 4,
  threads_per_chain = 1,
  adapt_delta = 0.99,
  max_treedepth = 15,
  backend = c("cmdstanr", "rstan"),
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  write_output = TRUE,
  output_path = "data/proc",
  input_dir = "data/proc",
  save_pars = TRUE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  A `brms_bym2_data_prep` object, data frame, prepared RDS path, or
  `NULL`.

- formula:

  Formula or character string. The BYM2 term is added automatically when
  the formula does not contain `car()`.

- cellsize:

  Numeric hex-grid cell size in metres, or an H3 specification such as
  `"h3_9"`.

- temporal_resolution:

  Either `"daily"` or `"hourly"`.

- adjacency:

  Optional adjacency matrix when preparing raw data.

- adjacency_args:

  Additional adjacency-builder arguments.

- priors:

  Optional brms prior specification.

- nchains:

  Number of MCMC chains.

- threads_per_chain:

  Number of threads per chain.

- adapt_delta:

  Target acceptance probability.

- max_treedepth:

  Maximum NUTS tree depth.

- backend:

  Either `"cmdstanr"` or `"rstan"`.

- iso3, admin_level, admin_name:

  Location identifiers.

- write_output:

  Whether to save the fitted model.

- output_path:

  Output directory or RDS filename.

- input_dir:

  Directory containing prepared data.

- save_pars:

  Whether to save latent parameters.

- verbose:

  Whether to emit progress messages.

## Value

A fitted `brmsfit` object.
