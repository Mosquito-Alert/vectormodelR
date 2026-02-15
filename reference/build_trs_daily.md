# Build TRS daily sampling effort surface for a location

Clips the Mosquito Alert sampling effort dataset to an administrative
area already prepared by
[`initialize_ma_dataset()`](https://labs.mosquitoalert.com/mosquitoR/reference/initialize_ma_dataset.md),
writes the resulting TRS layer, and saves auxiliary artefacts (minimum
SE logit value and joined effort metrics). The required Mosquito Alert
vector presences are loaded from `vector_<slug>_malert.Rds`, allowing
the TRS artefact to be regenerated independently of the full
initialisation pipeline.

## Usage

``` r
build_trs_daily(
  iso3,
  admin_level,
  admin_name,
  sampling_effort_url =
    "https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz",
  vector_dir = "data/proc",
  data_dir = "data/proc",
  write_output = TRUE
)
```

## Arguments

- iso3:

  Three-letter ISO3 code identifying the country.

- admin_level:

  Administrative level used when the grid was created.

- admin_name:

  Administrative unit name.

- sampling_effort_url:

  Remote CSV (gzipped) providing the Mosquito Alert sampling effort
  surface. Defaults to the canonical GitHub source.

- vector_dir:

  Directory containing `vector_<slug>_malert.Rds`. Defaults to
  "data/proc".

- data_dir:

  Directory where perimeter artefacts live and where the TRS outputs
  should be written. Defaults to "data/proc".

- write_output:

  Logical; when `TRUE` (default) write the TRS artefacts to disk.

## Value

An `sf` object containing the clipped TRS daily surface, or `NULL` when
the artefact cannot be produced.

## Examples

``` r
if (FALSE) { # \dontrun{
artefacts <- build_trs_daily(
  iso3 = "ESP",
  admin_level = 4,
  admin_name = "Barcelona"
)
} # }
```
