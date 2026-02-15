# Compile ERA5 monthly CSVs from GRIB files (terra-only)

Scans `input_dir` for files named `era5_<iso3>_YYYY_MM_<var>.grib`,
converts each month into a single long-format CSV.GZ under
`processed/YYYY/`, and writes a recent N-month summary in `input_dir`.

## Usage

``` r
compile_era5_data_v2(
  input_dir = NULL,
  processed_dir = NULL,
  iso3 = NULL,
  recent_n = 3,
  verbose = TRUE
)
```

## Arguments

- input_dir:

  Directory containing per-variable GRIB files. If NULL/empty and `iso3`
  supplied, defaults to `file.path("data/weather/grib", tolower(iso3))`.

- processed_dir:

  Output dir for monthly CSVs. Defaults to `<input_dir>/processed`.

- iso3:

  Optional ISO3 code (character). Used to resolve `input_dir` (when
  missing) and to build output filenames.

- recent_n:

  Number of most recent monthly CSVs to merge into a summary (default
  3).

- verbose:

  Print progress.

## Value

(invisibly) a list with summary info.
