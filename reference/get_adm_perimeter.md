# Compute ADM polygon perimeters using local UTM projections

For each feature in an `sf` object, this helper estimates the most
appropriate UTM zone from the feature's centroid, reprojects the
geometry, and calculates both perimeter length and polygon area in
metres. Optionally, the resulting layer can be written to disk as an
`.rds` file under `data/proc`.

## Usage

``` r
get_adm_perimeter(
  sf_obj,
  output_filename = NULL,
  rds = TRUE,
  proc_dir = "data/proc",
  verbose = TRUE
)
```

## Arguments

- sf_obj:

  An `sf` object containing polygon geometries with a defined CRS.

- output_filename:

  Optional file name to use when persisting the result (when
  `rds = TRUE`). If `NULL`, defaults to `"perim_adm.rds"`.

- rds:

  Logical. If `TRUE`, save the computed layer as an `.rds` file under
  `proc_dir`. Defaults to `TRUE`.

- proc_dir:

  Directory where the `.rds` file should be written when `rds = TRUE`.
  Default is `"data/proc"`.

- verbose:

  Logical. Print messages when saving the `.rds` file. Defaults to TRUE.

## Value

An `sf` object with columns: `perimeter_m`, `area_m2`, and the original
`geometry` transformed back to EPSG:4326.

## Examples

``` r
if (FALSE) { # \dontrun{
gadm_sf <- get_gadm_data("ESP", 2, name = "Barcelona", rds = FALSE)
perim_sf <- get_adm_perimeter(gadm_sf, output_filename = "perim_barcelona.rds")
} # }
```
