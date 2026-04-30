# Download ERA5 climate data from the Copernicus Climate Data Store

Download ERA5 climate data from the Copernicus Climate Data Store

## Usage

``` r
get_era5_data(
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  bbox_margin_deg = 0.5,
  bounding_box = NULL,
  output_dir = NULL,
  dataset = "reanalysis-era5-single-levels",
  variables = c("2m_dewpoint_temperature", "2m_temperature", "10m_u_component_of_wind",
    "10m_v_component_of_wind", "surface_pressure", "total_precipitation"),
  start_ym = format(Sys.Date(), "%Y_%m"),
  end_ym = format(Sys.Date(), "%Y_%m"),
  ecmwfr_key = Sys.getenv("ECMWFR_KEY"),
  ecmwfr_user = "ecmwfr",
  write_key = FALSE,
  hours = sprintf("%02d:00", 0:23),
  retry = 2,
  pause_between_requests_sec = 90,
  pause_sec = 600,
  verbose = FALSE,
  return_files = FALSE
)
```

## Arguments

- iso3:

  character. ISO3 country code (e.g., "BGD", "ESP", "USA"). When
  `admin_level`/`admin_name` are also supplied, the download bbox is
  derived from the matching GADM unit.

- admin_level:

  integer. GADM administrative level (0=country, 1=region, 2=province,
  ...). Used only when `admin_name` is supplied.

- admin_name:

  character. Exact `NAME_<level>` value to select within GADM. When
  provided, `get_era5_data()` downloads a bbox around that admin unit.

- bbox_margin_deg:

  numeric. Margin (in degrees) added on each side of the derived bbox.
  Defaults to 0.5.

- bounding_box:

  numeric(4). c(north, west, south, east) in decimal degrees. Ignored if
  `iso3` is provided.

- output_dir:

  character. Directory where downloaded files will be saved. Default
  NULL uses `data/weather/grib/<iso3>` for country-wide downloads, or
  `data/weather/grib/<iso3>_<admin_level>_<admin_name>` when
  `admin_name` is provided.

- dataset:

  character. One of "reanalysis-era5-single-levels" or
  "reanalysis-era5-land".

- variables:

  character(). ERA5 variable short names. Default common surface vars.

- start_ym:

  character. Starting year-month in "YYYY_MM" format, e.g. "2024_05".
  Default = current year-month.

- end_ym:

  character. Ending year-month in "YYYY_MM" format, e.g. "2024_11".
  Default = current year-month.

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

- hours:

  character(). Hours like "00:00"…"23:00". Default all 24 hours.

- retry:

  integer. Number of retries after the first attempt. Default 2. Retries
  are only used when no CDS job URL was created.

- pause_between_requests_sec:

  numeric. Seconds to wait after a successful download before sending
  the next request.

- pause_sec:

  numeric. Seconds to wait between failed attempts. Default 600.

- verbose:

  logical. Pass to `wf_request(verbose=)`. Default FALSE.

- return_files:

  logical. If TRUE, returns a list with `summary` and `files`, otherwise
  just `summary`.

## Value

A list with `summary` and `files`. A CSV log is also written to
`<output_dir>/logs/`.
