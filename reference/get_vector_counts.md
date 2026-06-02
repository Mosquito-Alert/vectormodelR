# Aggregate Mosquito Alert + GBIF occurrence counts by GADM administrative level

Spatially joins Mosquito Alert (MA) and GBIF occurrence points to GADM
polygons and returns counts by admin name at the chosen level, plus
parent admin names and totals.

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

  Character ISO3 code, e.g. `"MEX"` or `"JAM"`.

- level:

  Integer requested GADM level. `0 = country`, `1 = region`,
  `2 = district`, etc.

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
  Default: `"github"`.

- taxon_key:

  Optional vector of GBIF taxon keys passed to
  [`vectormodelR::get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md).

- gbif_clip_to_perimeter:

  Logical passed to
  [`vectormodelR::get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md).

- gbif_save_outputs:

  Logical passed to
  [`vectormodelR::get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md).

- crs:

  Integer EPSG for point creation and spatial joining. Default: `4326`.

- join_predicate:

  Spatial predicate for join. Default:
  [`sf::st_within`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).
  You can set
  [`sf::st_intersects`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
  if you prefer edge inclusion.

- keep_unmatched:

  Logical. If TRUE, includes an `"UNMATCHED"` bucket for points that do
  not fall inside polygons. Default FALSE.

## Value

A tibble with columns:

- Parent admin columns, where relevant, e.g. `admin_level_1`,
  `admin_level_2`

- `admin_name`: the selected admin name at the requested/fallback level

- `malert_count`

- `gbif_count`

- `total`

- `iso3`

- `level_used`: actual level used after fallback

- `name_col`: which GADM `NAME_*` column was used as `admin_name`

## Details

If the requested GADM level is not available for the given ISO3, the
function falls back to the highest available GADM level.

Parent admin names are included for levels above 1. For example:

- If `level = 2`, output includes `admin_level_1` and `admin_name`.

- If `level = 3`, output includes `admin_level_1`, `admin_level_2`, and
  `admin_name`.

- If `level = 1`, output includes only `admin_name`.

Country-level names are not included as parent columns.

## Examples

``` r
if (FALSE) { # \dontrun{
counts <- get_vector_counts(iso3 = "MEX", level = 2)
counts <- get_vector_counts(iso3 = "JAM", level = 4) # falls back if 4 unavailable
} # }
```
