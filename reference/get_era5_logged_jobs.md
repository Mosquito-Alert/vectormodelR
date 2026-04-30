# Retry ERA5 downloads from a CDS job log

Reads a log created by
[`get_era5_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_era5_data.md),
finds rows with saved CDS job URLs, and attempts to transfer the
existing CDS jobs once. The log is updated in place.

## Usage

``` r
get_era5_logged_jobs(
  iso3,
  admin_level = NULL,
  admin_name = NULL,
  start_ym,
  end_ym,
  output_dir = NULL,
  dataset = "reanalysis-era5-single-levels",
  status_to_retry = c("job_created_not_downloaded", "submitted_not_downloaded",
    "failed_transfer"),
  verbose = TRUE
)
```

## Arguments

- iso3:

  Character. ISO3 country code.

- admin_level:

  Integer or NULL. GADM administrative level.

- admin_name:

  Character or NULL. GADM admin name.

- start_ym:

  Character. Starting year-month in "YYYY_MM" format.

- end_ym:

  Character. Ending year-month in "YYYY_MM" format.

- output_dir:

  Character or NULL. Directory where downloaded files are stored.

- dataset:

  Character. ERA5 dataset to use.

- status_to_retry:

  Character vector. Status values to retry.

- verbose:

  Logical. If TRUE, prints progress messages.

## Value

A list with `summary`, and/or `files`, and `log_file`.
