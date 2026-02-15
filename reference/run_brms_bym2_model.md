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

  Either an in-memory data frame or the path to the RDS file containing
  the model-preparation dataset.

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

  Optional prior specification created with `brms::set_prior()`.
  Defaults to `NULL`, which uses the brms defaults.

- nchains:

  Number of MCMC chains to run. Defaults to 4.

- threads_per_chain:

  Number of within-chain threads (requires the `cmdstanr` backend).
  Defaults to 1 (single-threaded chains).

- adapt_delta:

  Target acceptance rate for the NUTS sampler. Defaults to 0.99.

- max_treedepth:

  Maximum tree depth for the NUTS sampler. Defaults to 12.

- backend:

  Sampling backend passed to `brms::brm()`. Defaults to `"cmdstanr"` but
  `"rstan"` is supported as well.

- iso3:

  Optional ISO3 country code used to derive the output filename via
  `build_location_identifiers()`. Must be supplied alongside
  `admin_level` and `admin_name` when provided.

- admin_level:

  Administrative level corresponding to the modelling dataset. Used for
  output naming when paired with `iso3` and `admin_name`.

- admin_name:

  Administrative unit name corresponding to the modelling dataset. Used
  for output naming when paired with `iso3` and `admin_level`.

- write_output:

  Logical; when `TRUE` the fitted model is saved to disk using either
  the supplied `output_path` (treated as a directory when it exists or
  lacks an extension) or the directory inferred from `dataset`, with
  filenames of the form `model_<slug>_brms.Rds` where possible. Defaults
  to `TRUE`.

- output_path:

  Optional location where the fitted model should be written. Supply
  either a directory or a full file path. If `NULL` (default) and
  `write_output` is `TRUE`, the path is auto-derived from `dataset` when
  supplied as a file path.

- save_pars:

  Logical; forwarded to `brms::brm()` as the `save_pars` argument.
  Defaults to `TRUE` to retain latent BYM2 parameters.

- verbose:

  Logical; if `TRUE`, progress messages are emitted. Defaults to `TRUE`.

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
