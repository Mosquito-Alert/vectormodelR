# Sequentially enrich model-preparation datasets

Locates a prepared model dataset and adds requested features in the
supplied order. Weather resolution is controlled by
`temporal_resolution`. When `se` is requested, the most recent preceding
grid feature determines the shared cell-ID column: `h3_<resolution>`
maps to `h3_id_<resolution>`, and `hex_<cellsize>` maps to
`hex_id_<cellsize>`.

## Usage

``` r
add_features(
  iso3,
  admin_level,
  admin_name,
  features,
  temporal_resolution = c("daily", "hourly"),
  vector_sources = c("malert", "gbif"),
  data_dir = "data/proc",
  verbose = TRUE
)
```

## Arguments

- iso3:

  Three-letter ISO3 country code.

- admin_level:

  Administrative level used when preparing the dataset.

- admin_name:

  Administrative unit name.

- features:

  Character vector or comma-separated feature codes. Available codes are
  `"hex"`, `"hex_<cellsize>"`, `"h3_<resolution>"`, `"wx_land"`,
  `"wx_single"`, `"lc"`, `"ndvi"`, `"el"`, `"pd"`, `"se"`,
  `"se_<factor>"`, and `"se_<trs_factor>_<tgb_factor>"`.

- temporal_resolution:

  Either `"daily"` or `"hourly"`. This controls weather enrichment and
  pseudoabsence generation.

- vector_sources:

  Vector data sources used to prepare the base dataset. Accepted values
  are `"malert"` and `"gbif"`.

- data_dir:

  Directory containing processed data.

- verbose:

  Logical. Print progress messages.

## Value

The enriched dataset.

## Details

Sampling-effort codes can optionally specify pseudoabsence sampling
factors: `"se"` uses the defaults from
[`add_pseudoabsences_se()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_pseudoabsences_se.md),
`"se_7"` uses 7 for both TRS and TGB, and `"se_7_5"` uses 7 for TRS and
5 for TGB.

## Examples

``` r
if (FALSE) { # \dontrun{
daily_data <- add_features(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona",
  temporal_resolution = "daily",
  features = "h3_9,se_7,el,pd,wx_land,ndvi,lc"
)

hourly_data <- add_features(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona",
  temporal_resolution = "hourly",
  features = "hex_1200,se_7_5,el,pd,wx_land,ndvi,lc"
)
} # }
```
