# Get GADM attributes for a country/level

Downloads a GADM layer via geodata, returns its attributes as either a
regular data.frame or an interactive DT datatable (if available).

## Usage

``` r
get_gadm_names(
  country = "Spain",
  level = 2,
  path = "data/gadm",
  view = c("datatable", "table")
)
```

## Arguments

- country:

  Character scalar. Country name as accepted by
  [`geodata::gadm()`](https://rdrr.io/pkg/geodata/man/gadm.html) (e.g.
  "Spain").

- level:

  Integer. Administrative level (0 = country, 1 = region, 2 = province,
  3 = municipality, etc.).

- path:

  Character. Directory where GADM files will be cached/downloaded.

- view:

  Either `"datatable"` (default, requires DT) or `"table"` for a plain
  data.frame.

## Value

A `data.frame` of GADM attributes, or an interactive datatable object if
`view = "datatable"` and DT is installed.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Provinces of Spain as interactive table
  explore_gadm_names("Spain", level = 2)

  # Municipalities of Spain as a regular data.frame
  df <- explore_gadm_names("Spain", level = 3, view = "table")
  head(df)
} # }
```
