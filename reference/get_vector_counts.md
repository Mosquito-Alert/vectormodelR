# Aggregate Mosquito Alert + GBIF occurrence counts by GADM administrative level

Spatially joins Mosquito Alert (MA) and GBIF occurrence points to GADM
polygons and returns counts by admin name at the chosen level, plus
totals.

## Usage

``` r
get_vector_counts(
  iso3,
  level = 2,
  gadm = NULL,
  gbif_tbl = NULL,
  malert_sf = NULL,
  malert_source = "github",
  taxon_key = NULL,
  gbif_clip_to_perimeter = FALSE,
  gbif_save_outputs = FALSE,
  crs = 4326,
  join_predicate = sf::st_within,
  keep_unmatched = FALSE
)
```

## Arguments

- iso3:

  Character ISO3 code (e.g., "MEX", "JAM").

- level:

  Integer requested GADM level (0 = country, 1 = region, 2 = district,
  ...).

- gadm:

  Optional sf polygon layer for the chosen GADM level. If NULL, fetched
  via `vectormodelR::get_gadm_data(iso3, level)`.

- gbif_tbl:

  Optional data.frame/tibble of GBIF occurrences. If NULL, fetched via
  [`vectormodelR::get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md).

- malert_sf:

  Optional sf POINT layer for Mosquito Alert occurrences. If NULL,
  fetched via
  [`vectormodelR::get_malert_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_malert_data.md).

- malert_source:

  Character passed to `vectormodelR::get_malert_data(source = ...)`.
  Default: "github".

- taxon_key:

  Optional vector of GBIF taxon key passed to
  [`vectormodelR::get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md).

- gbif_clip_to_perimeter:

  Logical passed to
  [`vectormodelR::get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md).

- gbif_save_outputs:

  Logical passed to
  [`vectormodelR::get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md).

- crs:

  Integer EPSG for point creation / joining. Default 4326 (WGS84).

- join_predicate:

  Spatial predicate for join. Default
  [`sf::st_within`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).
  You can set
  [`sf::st_intersects`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
  if you prefer edge-inclusion.

- keep_unmatched:

  Logical. If TRUE, includes an "UNMATCHED" bucket for points that do
  not fall inside polygons. Default FALSE (drops NAs).

## Value

A tibble with columns:

- `admin_name` : the selected name column at the level (e.g., NAME_2)

- `malert_count`

- `gbif_count`

- `total`

- `iso3`

- `level_used` : actual level used after fallback

- `name_col` : which NAME\_\* column was used

## Details

If the requested GADM level is not available for the given ISO3, the
function falls back to the highest available GADM level.

## Examples

``` r
if (FALSE) { # \dontrun{
counts <- get_vector_counts(iso3 = "MEX", level = 2)
counts <- get_vector_counts(iso3 = "JAM", level = 4) # falls back if 4 unavailable
} # }
```
