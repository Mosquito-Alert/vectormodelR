# Mosquito Alert Modeling Pipeline

## Overview

This vignette walks through the end-to-end Mosquito Alert modeling
pipeline implemented in the **mosquitoR** package. It covers required
inputs, intermediate artifacts, helper functions, file naming
conventions, and the final reporting step so you can reproduce or adapt
the workflow for a new location.

The canonical pipeline generates a feature-enriched dataset, fits a
Bernoulli occupancy model with `brms`, and renders a diagnostic report:

``` r
library(mosquitoR)

# Location configuration
iso3 <- "ITA"
admin_level <- 3
admin_name <- "Roma"

# Model runtime options
nchains <- 4
threads_per_chain <- 2

# External data locations/keys (examples)
ecmwfr_key <- Sys.getenv("ECMWFR_KEY")
raw_landcover <- "data/raw/landcover_italy.tif"
raw_ndvi <- "data/raw/ndvi_italy.tif"
raw_pop_density <- "data/raw/worldpop_italy.tif"

get_era5_data(
  country_iso3 = iso3,
  start_year   = 2014,
  end_year     = 2025,
  ecmwfr_key   = ecmwfr_key,
  write_key    = TRUE,
  data_format  = "grib",
  verbose      = TRUE
)

compile_era5_data_v2(
  iso3     = iso3,
  recent_n = 12,
  verbose  = TRUE
)

process_era5_data(
  iso3             = iso3,
  admin_level      = admin_level,
  admin_name       = admin_name,
  aggregation_unit = "cell"
)

map <- get_gadm_data(
  iso3      = iso3,
  name      = admin_name,
  level     = admin_level,
  perimeter = TRUE,
  rds       = TRUE
)

hex_grid <- build_spatial_grid(
  iso3        = iso3,
  admin_level = admin_level,
  admin_name  = admin_name,
  shape       = "hex",
  write       = TRUE
)

processed_lc <- process_landcover_data(
  terra::rast(raw_landcover),
  iso3        = iso3,
  admin_level = admin_level,
  admin_name  = admin_name,
  proc_dir    = "data/proc",
  verbose     = TRUE
)

elevation <- get_elevation_data(
  iso3       = iso3,
  level      = admin_level,
  name_value = admin_name
)

processed_ndvi <- process_ndvi_data(
  terra::rast(raw_ndvi),
  iso3,
  admin_level,
  admin_name
)

processed_pop_density <- process_popdensity_data(
  terra::rast(raw_pop_density),
  iso3,
  admin_level,
  admin_name
)

initialized_data <- initialize_ma_dataset(iso3, admin_level, admin_name)

enriched_data <- add_features(
  iso3,
  admin_level,
  admin_name,
  "se,el,ndvi,pd,wx,lc,hex",
  verbose = TRUE
)

brms_model <- run_brms_model(
  enriched_data,
  nchains           = nchains,
  threads_per_chain = threads_per_chain,
  backend           = "cmdstanr",
  iso3              = iso3,
  admin_level       = admin_level,
  admin_name        = admin_name,
  write_output      = TRUE,
  output_path       = "data/proc",
  verbose           = TRUE
)

interpret_brms_model(brms_model)
```

The sections below unpack each component, provide implementation notes,
and highlight where files are written on disk.

## Directory conventions and naming

Most helpers default to storing outputs under `data/proc`. Filenames
follow the slug produced by
`build_location_identifiers(iso3, admin_level, admin_name)`:

- Administrative boundary: `spatial_<slug>_adm.Rds`
- Spatial grid: `spatial_<slug>_<shape>_grid_<cellsize>.Rds`
- Feature-prepared dataset: `model_prep_<slug>_<suffixes>.Rds`
- Fitted model: `model_<slug>_brms.Rds`
- Diagnostic report: `model_<slug>_brms_report.<ext>`

Keep raw rasters (land cover, NDVI, population density) in `data/raw`.
You can customize directories via function arguments (`data_dir`,
`proc_dir`, or `output_path`) when needed.

## Weather ingestion (ERA5)

1.  [`get_era5_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_era5_data.md)
    downloads hourly ERA5 fields using the ECMWF Web API, storing GRIB
    files in `data/raw/era5`.
2.  [`compile_era5_data_v2()`](https://labs.mosquitoalert.com/mosquitoR/reference/compile_era5_data_v2.md)
    mosaics the tiles, clones the latest `recent_n` months, and caches
    intermediate NetCDF/Parquet files in `data/proc/era5`.
3.  [`process_era5_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_era5_data.md)
    aggregates weather summaries to `cell` or `hex` spatial units,
    producing statistics like daily maximum temperature and 24-hour
    precipitation totals that align with modeling features.

Make sure your ECMWF key is registered and available before running
these steps. Re-running only downloads new months when data already
exists.

## Administrative boundaries

[`get_gadm_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gadm_data.md)
retrieves polygons from GADM (or reuses cached files). Setting
`perimeter = TRUE` simplifies geometries, and `rds = TRUE` ensures the
result is written as `spatial_<slug>_adm.Rds`. When a map is already
loaded, pass it via `map` to avoid refetching.

## Spatial grid creation

[`build_spatial_grid()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_spatial_grid.md)
generates the modeling lattice. Supply either a map or ISO inputs; the
function automatically reads `spatial_<slug>_adm.Rds`. Key parameters:

- `cellsize_m`: grid spacing in meters
- `clip`: intersect hexes/squares with the boundary (default TRUE)
- `shape`: choose between “hex” and “square”

When `write = TRUE`, the grid is saved as
`spatial_<slug>_<shape>_grid_<cellsize>.Rds` inside `data/proc`. The
saved object is an `sf` tibble containing `grid_id`, centroid
coordinates, and the geometry column.

## Environmental covariates

### Land cover

[`process_landcover_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_landcover_data.md)
reprojects, crops, and tabulates land-cover classes. It writes cleaned
rasters to `data/proc/landcover` and produces per-grid coverage
proportions consumed later by
[`add_landcover_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_landcover_features.md).

### Elevation

[`get_elevation_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_elevation_data.md)
downloads elevation tiles, mosaics them, and derives summary statistics
(mean, min, max) for each grid cell. Outputs live under
`data/proc/elevation`.

### NDVI

[`process_ndvi_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_ndvi_data.md)
ingests NDVI composites, computing proximity distances to vegetation
(`ndvi_ldist_m`) and summary statistics (`ndvi_mean`, `ndvi_sd`). The
function returns both processed rasters and per-grid tables ready for
joins.

### Population density

[`process_popdensity_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_popdensity_data.md)
harmonizes WorldPop-style rasters, rescales density units, and
aggregates counts to each grid (`popdensity_km2`).

Each processor respects slug-based naming so downstream helpers can
locate the correct files automatically.

## Dataset initialization

[`initialize_ma_dataset()`](https://labs.mosquitoalert.com/mosquitoR/reference/initialize_ma_dataset.md)
combines Mosquito Alert reports with spatial indices:

1.  Reads the base dataset (`model_prep_<slug>_base.Rds`).
2.  Joins grid identifiers and centroids.
3.  Adds temporal features (`date`, `year`, `biweek`, `day_of_year`).
4.  Records metadata such as `location_slug` and `output_path` for later
    steps.

## Feature enrichment

[`add_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_features.md)
applies enrichment helpers in sequence. Supported codes:

- `se`: seasonality and pseudoabsence indicators
- `el`: elevation summaries
- `ndvi`: NDVI proximity metrics
- `pd`: population-density gradients
- `wx`: ERA5 weather summaries
- `lc`: land-cover proportions
- `hex` / `hex_<cellsize>`: geometry columns (`grid_lon`, `grid_lat`,
  `geometry`)

Helpers update the dataset and its metadata; the final helper writes the
enriched frame to the path stored in `output_path` (for example,
`model_prep_<slug>_base_el_ndvi_pd_wx_lc_hex400.Rds`). Check
`attr(enriched_data, "output_path")` to confirm the destination.

## Model fitting

[`run_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_model.md)
fits the baseline Bernoulli occupancy model. Internally it:

1.  Validates required columns and scales covariates (`maxTM_z`,
    `ppt_z`, `ndvi_z`, `elev_z`, `pop_z`).
2.  Builds a formula with a cyclic smooth on `day_of_year`, a smooth for
    maximum temperature, linear effects for other fixed predictors, and
    random intercepts for `year` and `landcover_code`.
3.  Calls `brms::brm()` using the requested backend (`cmdstanr` by
    default).
4.  Stores metadata (slug, output path, source dataset) on the fitted
    object.

Set `write_output = TRUE` and provide `output_path` to persist the model
as `model_<slug>_brms.Rds`. Adjust `priors`, `iter`, and sampler
controls to suit your analysis.

## Model interpretation

[`interpret_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/interpret_brms_model.md)
renders a diagnostic report that includes:

- Metadata and sampler diagnostics (R-hat, effective sample sizes,
  divergences)
- Tables for fixed, random, and smooth terms
- Conditional effect plots constructed from stored prediction grids
- Land-cover random-effect summaries
- Neutral narrative text describing parameter estimates

Reports default to HTML (`model_<slug>_brms_report.html`) but PDF/Word
outputs are available via the `format` argument.

## Automation tips

- Wrap the pipeline in scripts or `targets` plans to iterate over
  multiple locations.
- Cache heavy rasters in `data/proc` to avoid repetitive processing.
- Set `verbose = TRUE` to log every file that gets written.
- Use `build_location_identifiers()` in custom helpers to preserve
  naming consistency.
- Explore posterior draws with `posterior` or `bayesplot` for deeper
  diagnostics once the model is fit.

## Session information

``` r
sessionInfo()
```
