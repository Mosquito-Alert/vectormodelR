# Compile ERA5 monthly CSVs from multi-variable GRIB/ZIP files

Scans `input_dir` for ERA5 files (GRIB or ZIP format, with all variables
in one file per month) and converts each month into a single long-format
CSV.GZ. Output is organized by dataset type:

- ERA5 Single Levels → `processed/single-levels/YYYY/`

- ERA5-Land → `processed/land/YYYY/`

## Usage

``` r
compile_era5_data(
  input_dir = NULL,
  processed_dir = NULL,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  dataset = "reanalysis-era5-single-levels",
  recent_n = 3,
  verbose = TRUE
)
```

## Arguments

- input_dir:

  Directory containing multi-variable GRIB files. If NULL/empty and
  `iso3` supplied, defaults to
  `file.path("data/weather/grib", tolower(iso3))` or
  `file.path("data/weather/grib", <slug>)` when `admin_name` is
  provided.

- processed_dir:

  Output dir for monthly CSVs. Defaults to `<input_dir>/processed`.
  Within this, dataset-specific subdirectories are created: `land/` and
  `single-levels/`.

- iso3:

  Optional ISO3 code (character). Used to resolve `input_dir` (when
  missing) and to build output filenames.

- admin_level:

  integer. GADM administrative level (0=country, 1=region, 2=province,
  ...). Used only when `admin_name` is supplied.

- admin_name:

  character. Exact `NAME_<level>` value to select within GADM. When
  provided, uses admin-specific subdirectory and file naming.

- dataset:

  character. ERA5 dataset name ("reanalysis-era5-single-levels" or
  "reanalysis-era5-land"). Required parameter - specifies which dataset
  to process.

- recent_n:

  Number of most recent monthly CSVs to merge into a summary (default
  3).

- verbose:

  Print progress.

## Value

(invisibly) a list with summary info.

## Details

This version handles the new CDS API format where all variables are
bundled into a single file per month (e.g.,
era5_ita_3_firenze_2022_12.grib containing all variables).

Also writes recent N-month summaries in `input_dir`.
