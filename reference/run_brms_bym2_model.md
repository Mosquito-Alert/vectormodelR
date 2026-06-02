# Fit a BYM2 Mosquito Alert occupancy model with brms

Fits a Bayesian occupancy model using `brms` with a spatial BYM2/CAR
random effect based on a grid adjacency matrix.

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

  An in-memory modelling dataset, a `brms_data_prep` object, a
  `bym2_data_prep` object, a data.frame, a path to a prepared RDS file,
  or `NULL`. If `NULL`, `iso3`, `admin_level`, and `admin_name` are used
  to locate a prepared BYM2 object in `input_dir`.

- formula:

  Character string or formula object specifying the fixed and
  non-spatial random effects structure. This is required. If the formula
  does not contain a `car()` term, the BYM2 term
  `+ car(W, gr = <grid_col>, type = "bym2")` is appended automatically.
  If the formula already contains `car(...)`, it is used as supplied.

- cellsize_m:

  Numeric cell size in meters. Defaults to `800`, expecting a grid
  column such as `grid_id_800`.

- temporal_resolution:

  Character. Either `"daily"` or `"hourly"`. Determines which
  prepared-file name is used when `dataset = NULL`.

- adjacency:

  Optional pre-computed adjacency matrix whose row/column names match
  the grid identifier column.

- adjacency_args:

  Named list of additional arguments forwarded to
  [`build_grid_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_grid_adjacency.md)
  when adjacency needs to be built.

- priors:

  Optional `brms::set_prior` object. If `NULL`, default BYM2 priors are
  used.

- nchains:

  Integer. Number of MCMC chains.

- threads_per_chain:

  Integer. Number of threads per chain.

- adapt_delta:

  Numeric. Target average proposal acceptance probability.

- max_treedepth:

  Integer. Maximum tree depth for NUTS.

- backend:

  Character. `"cmdstanr"` or `"rstan"`.

- iso3, admin_level, admin_name:

  Optional location identifiers used for prepared-file lookup and
  adjacency construction.

- write_output:

  Logical. Whether to save the fitted model to disk.

- output_path:

  Directory or file path for saved model output.

- input_dir:

  Directory used when automatically locating prepared data.

- save_pars:

  Logical. Forwarded to `brms::brm()`.

- verbose:

  Logical. Emit messages when `TRUE`.

## Value

A fitted `brmsfit` object with attributes for model data, adjacency IDs,
grid column, source dataset, location slug, formula text, and output
path.

## Details

This function is the spatial counterpart to
[`run_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_model.md).
It expects either a `bym2_data_prep` object created by
[`prepare_bym2_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_bym2_data.md),
a path to a prepared BYM2 RDS file, a `brms_data_prep` object plus
enough information to build/provide adjacency, or a raw modelling data
frame that can be passed through
[`prepare_bym2_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_bym2_data.md).
