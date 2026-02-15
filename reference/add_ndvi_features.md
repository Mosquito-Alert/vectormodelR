# Add high-NDVI proximity metrics to Mosquito Alert model inputs

Loads the land-cover enriched model-preparation dataset produced by
[`add_landcover_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_landcover_features.md),
identifies raster cells whose NDVI exceeds a supplied threshold, and
measures each report's distance to the nearest qualifying cell. Results
are saved as `model_prep_*_wx_lc_ndvi.Rds`.

## Usage

``` r
add_ndvi_features(
  dataset,
  data_dir = "data/proc",
  ndvi_threshold = 0.3,
  decay_alpha = 0.01,
  decay_beta = 1,
  verbose = TRUE,
  write_output = TRUE
)
```

## Arguments

- dataset:

  Either the in-memory land-cover enriched dataset (output of
  [`add_landcover_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_landcover_features.md))
  or a path to the corresponding RDS file. When a data object is
  supplied it must carry an `output_path` attribute naming the most
  recently saved file; the NDVI-enriched output is written to `data_dir`
  with `_ndvi.Rds` appended to that stem when `write_output` is `TRUE`.

- data_dir:

  Directory containing processed rasters and where the output dataset
  will be written. Defaults to `"data/proc"`.

- ndvi_threshold:

  Numeric cutoff applied to the NDVI raster. Defaults to `0.3`.

- decay_alpha:

  Optional exponential decay coefficient used when deriving proximity;
  set to `NULL` to skip. Defaults to `0.01`.

- decay_beta:

  Exponent applied to the distance term in the decay function. Ignored
  when `decay_alpha` is `NULL`. Defaults to `1`.

- verbose:

  Logical; if `TRUE`, prints progress messages.

- write_output:

  Logical flag; when `TRUE` (default) the enriched dataset is written to
  disk. Set to `FALSE` to skip writing while still returning the
  augmented object and updating its metadata.

## Value

A tibble/data frame with the augmented variables. Attributes from the
source dataset are preserved and augmented with `ndvi_source`,
`ndvi_threshold`, and `output_path` metadata. ndvi_distance_m – meters
from each report to the closest raster cell whose NDVI ≥ threshold.
ndvi_value_nearest – NDVI value at that nearest qualifying cell.
ndvi_ddf_proximity – optional exponential decay score exp(-α ·
distance^β); higher values mean nearer high-NDVI areas.
