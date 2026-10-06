# Build TRS daily sampling effort surface for a location

Clips the Mosquito Alert sampling-effort dataset to an administrative
area, expands the original 0.025-degree effort cells onto a selected H3
or hex model grid, and saves the resulting daily TRS surface.

## Usage

``` r
build_trs_daily(
  iso3,
  admin_level,
  admin_name,
  grid = "h3_9",
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

- grid:

  Model grid code, such as `"h3_9"` or `"hex_1200"`.

- sampling_effort_url:

  Remote CSV providing the Mosquito Alert sampling effort surface.

- vector_dir:

  Directory containing `vector_<slug>_malert.Rds`.

- data_dir:

  Directory containing spatial inputs and TRS outputs.

- write_output:

  Whether to write the TRS artefacts.

## Value

An `sf` point object containing the expanded daily TRS surface.
