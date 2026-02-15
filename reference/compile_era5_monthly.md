# Compile ERA5 monthly CSVs from GRIB/NetCDF files

Scans `input_dir` for `era5_YYYY_MM_<var>.<ext>` files, converts each
month into a single long-format CSV.GZ under `processed/YYYY/`, and
writes a 3-month recency file in `input_dir`.

## Usage

``` r
compile_era5_monthly(
  input_dir,
  processed_dir = file.path(input_dir, "processed"),
  file_ext = c("grib", "nc"),
  prefer = c("terra", "stars"),
  recent_n = 3,
  verbose = TRUE
)
```

## Arguments

- input_dir:

  Directory containing per-variable files.

- processed_dir:

  Output directory for monthly CSVs (default: file.path(input_dir,
  "processed")).

- file_ext:

  "grib" or "nc".

- prefer:

  "terra" or "stars" (primary reader; the other is fallback).

- recent_n:

  Number of most recent monthly CSVs to merge into a summary (default
  3).

- verbose:

  Logical; print progress.

## Value

(Invisibly) a list with summary info.

## Examples

``` r
if (FALSE) { # \dontrun{
# Compile ERA5 monthly data from GRIB files in a directory
compile_era5_monthly(
  input_dir     = "data/era5/raw",
  file_ext      = "grib",
  prefer        = "terra",
  recent_n      = 3,
  verbose       = TRUE
)

# Compile ERA5 monthly data from NetCDF files
compile_era5_monthly(
  input_dir     = "data/era5_nc",
  file_ext      = "nc",
  prefer        = "stars",
  verbose       = FALSE
)

# After running, processed CSVs are available in processed/YYYY/
# and a recent 3-month combined file in input_dir.
} # }
```
