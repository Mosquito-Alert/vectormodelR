# Retrieve GADM polygons using geodata::gadm()

Fetches administrative polygons for a given country and level using
geodata. Optionally filters by a specific admin name (e.g., "Barcelona"
at level 2) and can return either all polygons or a single unioned
region. Matching behaviour can be toggled between substring search and
exact name matching.

## Usage

``` r
get_gadm_data(
  iso3,
  level,
  name = NULL,
  path = "data/gadm",
  rds = TRUE,
  perimeter = FALSE,
  union = FALSE,
  match_type = c("exact", "contains"),
  verbose = TRUE
)
```

## Arguments

- iso3:

  Character. ISO3 country code (e.g., "ESP").

- level:

  Integer. GADM administrative level (0 = country, 1 = region, 2 =
  province, ...).

- name:

  Character or NULL. Optional name or vector of names to filter by.
  Matching defaults to case-insensitive substring search but can be
  changed via `match_type`.

- path:

  Character. Directory for caching downloaded data. Default is
  `"data/gadm"`.

- rds:

  Logical. If TRUE, save the returned object as an `.rds` file in
  `data/proc`. Default TRUE.

- perimeter:

  Logical. If TRUE, augment the result with perimeter and area metrics
  via
  [`get_adm_perimeter()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_adm_perimeter.md).
  Default FALSE.

- union:

  Logical. If TRUE, union matched polygons into a single multipolygon
  (useful for masking rasters). Default FALSE.

- match_type:

  Character. Matching mode for `name`. Use "exact" for case-insensitive
  equality or "contains" (default) for substring matches.

- verbose:

  Logical. Print progress messages. Default TRUE.

## Value

An `sf` polygon layer (EPSG:4326).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Whole country of Spain
esp <- get_gadm_data("ESP", level = 0)

# 2) Barcelona province (level 2)
barca <- get_gadm_data("ESP", level = 2, name = "Barcelona")

# 3) Multiple provinces while also computing perimeter metrics
cat <- get_gadm_data(
  "ESP",
  level = 2,
  name = c("Barcelona", "Girona"),
  rds = FALSE,
  perimeter = TRUE
)
} # }
```
