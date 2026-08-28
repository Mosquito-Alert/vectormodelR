# Add BYM2 spatial data to prepared INLA data

Takes the output from
[`prepare_inla_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_inla_data.md),
builds or accepts an adjacency matrix, and creates the spatial and
space-time objects required by INLA.

## Usage

``` r
prepare_inla_bym2_data(
  dataset,
  cellsize = 800,
  adjacency = NULL,
  adjacency_args = list(),
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  output_dir = "data/proc",
  write = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Object returned by
  [`prepare_inla_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_inla_data.md).

- cellsize:

  Numeric hex-grid cell size in metres, or an H3 specification such as
  `"h3_9"`.

- adjacency:

  Optional precomputed adjacency matrix. When `NULL`, it is created with
  [`build_grid_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_grid_adjacency.md)
  or
  [`build_h3_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_h3_adjacency.md).

- adjacency_args:

  Additional arguments passed to the selected adjacency builder.

- iso3, admin_level, admin_name:

  Optional location identifiers.

- output_dir:

  Directory used when `write = TRUE`.

- write:

  Whether to save the prepared object.

- verbose:

  Whether to emit progress messages.

## Value

An `inla_bym2_data_prep` object containing the model data, adjacency
matrix, spatial graph, and space-time interaction objects.
