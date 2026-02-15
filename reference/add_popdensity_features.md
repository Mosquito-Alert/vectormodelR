# Add population density to Mosquito Alert model inputs

Loads the elevation-enriched model-preparation dataset produced by
[`add_elevation_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_elevation_features.md),
extracts population density values from the corresponding raster created
by
[`process_popdensity_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_popdensity_data.md),
and writes a new `model_prep_*_wx_lc_ndvi_elev_pd.Rds` file with a
`popdensity_km2` column (people per square kilometre).

## Usage

``` r
add_popdensity_features(
  dataset,
  data_dir = "data/proc",
  verbose = TRUE,
  write_output = TRUE
)
```

## Arguments

- dataset:

  Either the in-memory elevation-enriched dataset (output of
  [`add_elevation_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_elevation_features.md))
  or a path to the corresponding RDS file. When a data object is
  supplied it must carry an `output_path` attribute naming the most
  recently saved file; the population-density enriched dataset is
  written to `data_dir` with `_pd.Rds` appended to that stem when
  `write_output` is `TRUE`.

- data_dir:

  Directory holding the population density raster and where the enriched
  dataset will be written. Defaults to `"data/proc"`.

- verbose:

  Logical; if `TRUE`, prints status messages while processing.

- write_output:

  Logical flag; when `TRUE` (default) the enriched dataset is written to
  disk. Set to `FALSE` to skip writing while still returning the
  augmented object and updating its metadata.

## Value

A tibble/data frame containing the augmented dataset. Attributes from
the input object are preserved, and two additional attributes are set:
`popdensity_source` (the raster path) and `output_path` (the saved
file).

## Details

The population density raster is expected at
`file.path(data_dir, paste0("spatial_", slug, "_popdensity.tif"))`.
