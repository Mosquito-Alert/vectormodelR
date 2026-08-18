# Prepare base model data for INLA

Takes the object returned by
[`prepare_model_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_model_data.md)
and adds indices required by INLA. Hourly datasets also receive a cyclic
1-to-24 hour index. Continuous predictors can optionally be grouped for
INLA smooths.

## Usage

``` r
prepare_inla_data(
  dataset,
  landcover_reference = "Built-up",
  source_reference = NULL,
  group_specs = NULL,
  output_dir = "data/proc",
  write = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Object returned by
  [`prepare_model_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_model_data.md).

- landcover_reference:

  Reference level for `landcover_class`.

- source_reference:

  Optional reference level for `source`.

- group_specs:

  Optional named list of continuous predictors to group. Each
  specification must contain `input`, `output`, and `n`. The optional
  `method` defaults to `"quantile"`.

- output_dir:

  Directory used when `write = TRUE`.

- write:

  Whether to save the prepared object.

- verbose:

  Whether to emit progress messages.

## Value

An object of class `inla_data_prep`.
