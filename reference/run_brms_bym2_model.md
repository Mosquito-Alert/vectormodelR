# Fit a BYM2 Mosquito Alert occupancy model with brms

Fits a Bayesian occupancy model using `brms` with a BYM2 spatial random
effect based on a grid adjacency matrix.

## Usage

``` r
run_brms_bym2_model(
  dataset = NULL,
  formula,
  cellsize_m = 800,
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

  A `brms_bym2_data_prep` object, a data frame, a path to a prepared RDS
  file, or `NULL`. When `NULL`, location information is used to locate
  the prepared file in `input_dir`.

- formula:

  Formula or character string specifying the fixed and non-spatial
  random effects. The BYM2 term is added automatically when the formula
  does not already contain `car()`.

- cellsize_m:

  Numeric grid-cell size in metres.

- temporal_resolution:

  Either `"daily"` or `"hourly"`.

- adjacency:

  Optional adjacency matrix used when preparing a raw data frame.

- adjacency_args:

  Additional arguments passed to
  [`build_grid_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_grid_adjacency.md).

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

## Details

The function expects an object created by
[`prepare_brms_bym2_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_brms_bym2_data.md),
a path to a saved preparation object, or a raw data frame that can be
passed to
[`prepare_brms_bym2_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_brms_bym2_data.md).
