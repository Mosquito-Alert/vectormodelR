# Build an adjacency matrix for H3 cells

Uses the H3 identifiers in prepared model data to identify neighbouring
cells and returns a binary adjacency matrix.

## Usage

``` r
build_h3_adjacency(model, cellsize = "h3_9", sparse = TRUE)
```

## Arguments

- model:

  Model data or a path to a prepared model RDS file.

- cellsize:

  H3 specification such as `"h3_9"`.

- sparse:

  Logical. Return a sparse matrix when `TRUE`.

## Value

A binary adjacency matrix with H3 identifiers as row and column names.
