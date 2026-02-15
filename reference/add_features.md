# Sequentially enrich Mosquito Alert model-preparation datasets

Convenience wrapper that locates the base `model_prep_*_base.Rds` file
for a given location and applies one or more enrichment helpers
(hex-grid IDs, weather, landcover, NDVI, elevation, population density,
pseudoabsences) in the order supplied. Helpers run sequentially, with
the last step persisting the enriched dataset alongside the base inputs.
Intermediate helpers update metadata (notably the `output_path`
attribute) so suffixes accumulate as expected.

## Usage

``` r
add_features(
  iso3,
  admin_level,
  admin_name,
  features,
  vector_sources = c("malert", "gbif"),
  data_dir = "data/proc",
  verbose = TRUE
)
```

## Arguments

- iso3:

  Three-letter ISO3 country code.

- admin_level:

  Administrative level used when preparing the inputs.

- admin_name:

  Administrative unit name used in the file naming scheme.

- features:

  Character vector (or comma-separated string) indicating which feature
  steps to run. Accepted values: `"hex"`, `"hex_<cellsize>"` (for
  example `hex_800`), `"wx"`/`"weather"`, `"lc"`/`"landcover"`,
  `"ndvi"`, `"el"`/`"elevation"`, `"pd"`/`"popdensity"`,
  `"se"`/`"pseudoabsence"`.

- vector_sources:

  Character vector (or comma-separated string) describing which vector
  data sources were used to build the base dataset. Accepted values:
  "malert", "gbif". Determines the filename suffix that is located.

- data_dir:

  Directory containing the model-preparation datasets and derived
  artefacts. Defaults to `"data/proc"`.

- verbose:

  Logical; if `TRUE`, prints progress messages while processing.

- grid_cellsize_m:

  Numeric cell size (meters) corresponding to the stored hex grid.
  Defaults to 400.

## Value

The enriched dataset returned by the final helper that ran. The object
retains the `output_path` attribute referencing the file written by that
helper.
