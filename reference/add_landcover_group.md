# Add a supported land-cover grouping to prepared model data

Retains land-cover categories with enough presences and absences, and
groups sparse categories into "Other". If "Other" remains sparse, uses
Built-up versus all other land-cover classes.

## Usage

``` r
add_landcover_group(
  dataset,
  reference = "Built-up",
  min_presences = 10L,
  min_absences = 10L
)
```

## Arguments

- dataset:

  Output from
  [`prepare_model_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_model_data.md).

- reference:

  Reference land-cover category.

- min_presences:

  Minimum presences required to retain a category.

- min_absences:

  Minimum absences required to retain a category.

## Value

The prepared dataset with `landcover_group` added to `model_data`.
