# Add elevation to Mosquito Alert model inputs

Takes a model-preparation dataset, extracts elevation values from a
corresponding raster, and writes a new RDS file with an `elevation_m`
column and `_el` appended to the original filename when output is
enabled.

## Usage

``` r
add_elevation_features(
  dataset,
  data_dir = "data/proc",
  verbose = TRUE,
  write_output = TRUE
)
```

## Arguments

- dataset:

  Either the in-memory model-preparation dataset or a path to the
  corresponding RDS file. When a data object is provided it must carry
  an `output_path` attribute naming the most recently saved file; the
  elevation-enriched dataset is written to `data_dir` with `_el.Rds`
  appended to that stem when `write_output` is `TRUE`.

- data_dir:

  Directory that houses the processed rasters and where the
  elevation-enriched dataset will be written. Defaults to `"data/proc"`.

- verbose:

  Logical; if `TRUE`, prints status messages while processing.

- write_output:

  Logical flag; when `TRUE` (default) the enriched dataset is written to
  disk. Set to `FALSE` to skip writing while still returning the
  augmented object and updating its metadata.

## Value

A tibble/data frame containing the augmented dataset. Attributes from
the input object are preserved, and two additional attributes are set:
`elevation_source` (the raster path) and `output_path` (the saved file).

## Details

The elevation raster is expected at
`file.path(data_dir, paste0("spatial_", slug, "_elevation.tif"))`.
