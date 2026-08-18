# Build a Knorr-Held Type IV space-time interaction

Creates a spatial-by-year interaction using a Besag spatial structure
and an RW1 temporal structure. Disconnected spatial components are
handled separately. Isolated cells are excluded from the structured
interaction.

## Usage

``` r
build_inla_type4(adjacency, spatial_graph, spatial_id, year_id)
```

## Arguments

- adjacency:

  Aligned spatial adjacency matrix.

- spatial_graph:

  INLA graph created from `adjacency`.

- spatial_id:

  Integer spatial index for each observation.

- year_id:

  Year index for each observation.

## Value

A list containing the space-time index, precision matrix, constraints,
and supporting metadata.
