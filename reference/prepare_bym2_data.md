# Prepare data and adjacency matrix for BYM2 modelling

Cleans the dataset, scales predictors (storing means/SDs for future
use), and aligns the spatial adjacency matrix to the filtered data.
Optionally saves the prepared object to disk.

## Usage

``` r
prepare_bym2_data(
  dataset,
  cellsize_m = 800,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  adjacency = NULL,
  adjacency_args = list(),
  output_dir = "data/proc",
  write = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Either an in-memory data frame or the path to the RDS file containing
  the model-preparation dataset.

- cellsize_m:

  Numeric cell size (meters). Defaults to 800.

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

- adjacency:

  Optional pre-computed adjacency matrix. If NULL, one is built.

- adjacency_args:

  List of arguments passed to
  [`build_grid_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_grid_adjacency.md).

- output_dir:

  Directory where the prepared dataset is written when `write = TRUE`.
  Defaults to `"data/proc"`.

- write:

  Logical. If `TRUE` the prepared object is written to disk. Defaults to
  `FALSE`.

- verbose:

  Logical. Emit informative messages when `TRUE`.

## Value

A list of class `bym2_data_list` containing:

- model_data:

  The filtered, scaled data.frame ready for brms.

- adjacency:

  The aligned sparse matrix corresponding to `model_data`.

- grid_col:

  The name of the grid identifier column used.

- scaling:

  A list of mean/sd values used for scaling predictors.

- meta:

  Metadata (iso3, admin_level, etc.) for file naming.
