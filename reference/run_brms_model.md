# Fit a Standard Mosquito Alert occupancy model with brms

Fits a Bayesian GLM/GAM on the Mosquito Alert occupancy dataset using
`brms`. This model does NOT include the spatial BYM2 random effect. It
uses the same processed data structure as
[`run_brms_bym2_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_bym2_model.md)
but ignores the adjacency matrix.

## Usage

``` r
run_brms_model(
  dataset = NULL,
  formula,
  cellsize_m = 800,
  temporal_resolution = c("daily", "hourly"),
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

  An in-memory modelling dataset (data.frame), a `brms_data_prep`
  object, a `bym2_data_prep` object, or a path to a prepared RDS file.

- formula:

  Character string or formula object specifying the fixed and random
  effects structure. This parameter is required.

- priors:

  Optional `brms::set_prior` object. If `NULL`, default priors for
  intercepts, coefficients, and standard deviations are used.

- nchains:

  Integer. Number of MCMC chains. Default 4.

- threads_per_chain:

  Integer. Number of threads per chain for within-chain parallelism.

- adapt_delta:

  Numeric. Target average proposal acceptance probability. Default
  0.995.

- max_treedepth:

  Integer. Max tree depth for NUTS. Default 20.

- backend:

  Character. "cmdstanr" (default) or "rstan".

- iso3, admin_level, admin_name:

  Optional strings to locate the dataset if `dataset` is NULL.

- write_output:

  Logical. Whether to save the fitted model to disk.

- output_path, input_dir:

  Paths for output/input.

- save_pars:

  Logical. Forwarded to `brms::brm`.

- verbose:

  Logical.

## Value

The fitted `brmsfit` object.
