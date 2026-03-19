# Download ERA5 climate data from the Copernicus Climate Data Store

Download ERA5 climate data from the Copernicus Climate Data Store

## Usage

``` r
get_era5_data(
  country_iso3 = NULL,
  bounding_box = NULL,
  output_dir = NULL,
  dataset = "reanalysis-era5-single-levels",
  variables = c("2m_dewpoint_temperature", "2m_temperature", "10m_u_component_of_wind",
    "10m_v_component_of_wind", "surface_pressure", "total_precipitation"),
  start_year = as.integer(format(Sys.Date(), "%Y")),
  end_year = as.integer(format(Sys.Date(), "%Y")),
  ecmwfr_key = Sys.getenv("ECMWFR_KEY"),
  ecmwfr_user = "ecmwfr",
  write_key = FALSE,
  data_format = c("grib", "netcdf"),
  hours = sprintf("%02d:00", 0:23),
  skip_if_exists_mb = 1,
  retry = 2,
  pause_sec = 10,
  verbose = FALSE
)
```

## Arguments

- country_iso3:

  character. ISO3 country code (e.g., "BGD", "ESP", "USA"). Takes
  precedence over `bounding_box`.

- bounding_box:

  numeric(4). c(north, west, south, east) in decimal degrees. Ignored if
  `country_iso3` is provided.

- output_dir:

  character. Directory where downloaded files will be saved. Default
  NULL uses "data/weather/grib/" when `country_iso3` supplied.

- dataset:

  character. One of "reanalysis-era5-single-levels" or
  "reanalysis-era5-land".

- variables:

  character(). ERA5 variable short names. Default common surface vars.

- start_year:

  integer. Starting year. Default = current year.

- end_year:

  integer. Ending year. Default = current year.

- ecmwfr_key:

  character or NULL. If provided and `write_key = TRUE`, it will be
  saved via `wf_set_key()`. If NULL, will try
  `Sys.getenv("CDS_API_KEY")`. Otherwise requires a pre-existing keyring
  entry.

- ecmwfr_user:

  character. Keyring label to use with ecmwfr. Default "ecmwfr".

- write_key:

  logical. Persist `ecmwfr_key` to the keyring (off by default for
  public packages).

- data_format:

  character. "grib" or "netcdf". Default "grib".

- hours:

  character(). Hours like "00:00"…"23:00". Default all 24 hours.

- skip_if_exists_mb:

  numeric. Skip downloads if an existing file is larger than this size
  (MB). Default 1.

- retry:

  integer. Retries per file on transient errors. Default 2.

- pause_sec:

  numeric. Seconds to wait between retries. Default 10.

- verbose:

  logical. Pass to `wf_request(verbose=)`. Default FALSE.

## Value

A list with `summary` (counts/paths) and `files` (data.frame of per-file
results).

## Examples

``` r
if (FALSE) { # \dontrun{
# Using a country code (tries Natural Earth if get_bounding_boxes() not available)
get_era5_data(country_iso3 = "BGD", start_year = 2024, end_year = 2024)

# Using a custom bounding box
get_era5_data(bounding_box = c(26.995, 86.950, 20.204, 93.500),
              variables = "2m_temperature", start_year = 2024, end_year = 2024)
} # }
```
