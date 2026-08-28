# Sequentially enrich model-preparation datasets

Locates a prepared model dataset and adds the requested features in the
order supplied.

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

  Administrative level used when preparing the dataset.

- admin_name:

  Administrative unit name.

- features:

  Character vector or comma-separated feature codes. Available codes are
  `"hex"`, `"hex_<cellsize>"`, `"h3_<resolution>"`, `"wx_daily_land"`,
  `"wx_hourly_land"`, `"wx_daily_single"`, `"wx_hourly_single"`, `"lc"`,
  `"ndvi"`, `"el"`, `"pd"`, and `"se"`.

- vector_sources:

  Vector data sources used to prepare the base dataset. Accepted values
  are `"malert"` and `"gbif"`.

- data_dir:

  Directory containing processed data.

- verbose:

  Logical. Print progress messages.

## Value

The enriched dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
daily_data <- add_features(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona",
  features = "se,el,pd,wx_daily_land,ndvi,lc,h3_6"
)

hourly_data <- add_features(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona",
  features = "se,el,pd,wx_hourly_land,ndvi,lc,h3_6"
)
} # }
```
