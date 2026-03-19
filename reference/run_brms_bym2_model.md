# Fit a BYM2 Mosquito Alert occupancy model with brms

Extends
[`run_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_model.md)
with a spatial BYM2 random effect using an adjacency matrix derived from
the hex grid. The helper accepts either an in-memory modelling dataset
or a path to the enriched RDS file and will construct the required
adjacency matrix when only the location identifiers are supplied.
Predictors are scaled to match the baseline specification and duplicate
observations per grid-date-source-presence combination are dropped prior
to model fitting.

## Usage

``` r
run_brms_bym2_model(
  dataset = NULL,
  formula = NULL,
  cellsize_m = 800,
  adjacency = NULL,
  adjacency_args = list(),
  priors = NULL,
  nchains = 4,
  threads_per_chain = 1,
  adapt_delta = 0.995,
  max_treedepth = 20,
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

  An in-memory modelling dataset (data.frame), a `bym2_data_prep`
  object, or a path to the enriched RDS file.

- formula:

  Optional character string or formula object specifying the fixed and
  random effects structure. If provided, it overrides the default base
  formula. The spatial BYM2 term
  `+ car(W, gr = <grid_col>, type = "bym2")` will be automatically
  appended to this formula. If `NULL` (default), the function processes
  `source` validation and uses a set of default predictors including
  `sea_days`, `maxTM_z`, `ppt_z`, `ndvi_z`, `elev_z`, `pop_z`, `year`,
  and `landcover_code`.

- cellsize_m:

  Numeric cell size (meters) of the hex grid whose adjacency matrix will
  be used. Defaults to 800, expecting a `grid_id_800` column in the
  dataset.

- adjacency:

  Optional pre-computed adjacency matrix whose row/column names match
  the grid identifier column. When `NULL`, the helper calls
  [`build_grid_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_grid_adjacency.md).

- adjacency_args:

  Named list of additional arguments forwarded to
  [`build_grid_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_grid_adjacency.md)
  when `adjacency` is `NULL`. These values override the defaults
  assembled by the helper.

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

  Logical; forwarded to `brms::brm()` as the `save_pars` argument.
  Defaults to `TRUE` to retain latent BYM2 parameters.

- verbose:

  Logical.

## Value

The fitted `brmsfit` object with attributes for the modelling data,
aligned adjacency matrix, and saved path when written to disk.

## Examples

``` r
if (FALSE) { # \dontrun{
bym2_fit <- run_brms_bym2_model(
  dataset = "data/proc/model_prep_esp_4_barcelona_malert_gbif_se_el_ndvi_pd_wx_lc_hex400_hex800.Rds",
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona",
  cellsize_m = 800,
  nchains = 4,
  threads_per_chain = 2
)
} # }
```
