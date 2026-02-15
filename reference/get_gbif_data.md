# Download GBIF occurrence data for a taxon and country

Submits an authenticated GBIF download request for a given species and
country, waits for completion, retrieves the zipped archive, removes the
downloaded file after import, clips occurrences to the supplied
perimeter, and returns the parsed occurrence records. GBIF credentials
can be provided via arguments or pulled from the `GBIF_USER`,
`GBIF_PWD`, and `GBIF_EMAIL` environment variables.

## Usage

``` r
get_gbif_data(
  taxon_key,
  iso3,
  admin_level,
  admin_name,
  year_min = NULL,
  year_max = NULL,
  desired_cols = NULL,
  out_dir = "data/proc",
  perimeter_dir = "data/proc",
  gbif_user = Sys.getenv("GBIF_USER", unset = NA_character_),
  gbif_pwd = Sys.getenv("GBIF_PWD", unset = NA_character_),
  gbif_email = Sys.getenv("GBIF_EMAIL", unset = NA_character_),
  clip_to_perimeter = TRUE,
  save_outputs = TRUE,
  verbose = TRUE
)
```

## Arguments

- taxon_key:

  Numeric vector of GBIF taxon keys (e.g. `c(1651430, 1651891)` for
  *Aedes albopictus* and *Aedes aegypti*).

- iso3:

  Three-letter ISO country code (e.g. `"ESP"`). It is converted to the
  GBIF-required ISO2 code using `countrycode`.

- admin_level:

  Administrative level used when the perimeter was generated. If
  `clip_to_perimeter = FALSE`, this value is optional.

- admin_name:

  Administrative unit name used for the perimeter. If
  `clip_to_perimeter = FALSE`, this value is optional.

- year_min:

  Optional minimum occurrence year.

- year_max:

  Optional maximum occurrence year.

- desired_cols:

  Optional character vector (or list) of column names to retain after
  perimeter filtering. Matching is case-insensitive and accepts common
  typos for `occurrenceStatus`/`occurrenceID`. When `NULL` (default),
  all columns are preserved.

- out_dir:

  Directory where both the raw GBIF download and the perimeter-clipped
  table should be saved. Defaults to `"data/vector"`. The unfiltered
  results are written as `vector_<iso3>_gbif.Rds`; the perimeter-clipped
  output becomes `vector_<iso3>_<admin_level>_<admin_name>_gbif.Rds`
  (with the administrative name normalised to lowercase underscores).

- perimeter_dir:

  Directory containing the perimeter RDS artefacts. The function looks
  for `spatial_<slug>_perimeter.rds` in this location. Ignored when
  `clip_to_perimeter = FALSE`.

- gbif_user:

  GBIF username. Defaults to `Sys.getenv("GBIF_USER")`.

- gbif_pwd:

  GBIF password. Defaults to `Sys.getenv("GBIF_PWD")`.

- gbif_email:

  GBIF-registered email. Defaults to `Sys.getenv("GBIF_EMAIL")`.

- clip_to_perimeter:

  Logical; when `TRUE` (default) the download is intersected with the
  administrative perimeter. Set to `FALSE` to retain the countrywide
  results without requiring perimeter artefacts.

- save_outputs:

  Logical; when `TRUE` (default) the raw and clipped datasets are
  written to disk. Use `FALSE` to return the data in memory only.

- verbose:

  Logical; if `TRUE` (default) progress messages are printed.

## Value

Tibble of occurrence records. Metadata attributes include the GBIF
download key (`gbif_key`), the perimeter-clipped table path
(`gbif_table_path`), the raw unfiltered table path
(`gbif_raw_table_path`), the location slug (`location_slug`), and the
perimeter source path (`perimeter_source`).
