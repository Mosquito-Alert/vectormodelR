#' Extract CDS job URL from an ecmwfr/CDS error message
#'
#' @param err_text Character. Error text returned by ecmwfr/CDS.
#' @return Character CDS job URL, or NA_character_ if none is found.
#' @keywords internal
#' @noRd
extract_cds_job_url <- function(err_text) {
  m <- regexpr(
    paste0(
      "https://cds\\.climate\\.copernicus\\.eu/api/retrieve/v1/jobs/",
      "[0-9a-fA-F]{8}-",
      "[0-9a-fA-F]{4}-",
      "[0-9a-fA-F]{4}-",
      "[0-9a-fA-F]{4}-",
      "[0-9a-fA-F]{12}"
    ),
    err_text,
    perl = TRUE
  )

  if (m[1] == -1) {
    return(NA_character_)
  }

  regmatches(err_text, m)
}


#' Build an ERA5 download log path
#'
#' @param output_dir Directory where ERA5 files are stored.
#' @param iso3 ISO3 code.
#' @param admin_level Administrative level or NULL.
#' @param admin_name Administrative name or NULL.
#' @param start_ym Start year-month in YYYY_MM format.
#' @param end_ym End year-month in YYYY_MM format.
#' @param dataset ERA5 dataset name.
#' @return Full CSV log path.
#' @keywords internal
#' @noRd
build_era5_log_path <- function(
  output_dir,
  iso3,
  admin_level,
  admin_name,
  start_ym,
  end_ym,
  dataset
) {
  log_dir <- file.path(output_dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  dataset_token <- if (identical(dataset, "reanalysis-era5-land")) {
    "era5land"
  } else if (identical(dataset, "reanalysis-era5-single-levels")) {
    "era5"
  } else {
    stop("Unknown ERA5 dataset: ", dataset, call. = FALSE)
  }

  iso3_slug <- tolower(iso3)

  location_slug <- if (!is.null(admin_name) && nzchar(admin_name)) {
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    ids$slug
  } else {
    iso3_slug
  }

  start_token <- gsub("_", "", start_ym)
  end_token <- gsub("_", "", end_ym)

  file.path(
    log_dir,
    paste0(
      "log_",
      dataset_token,
      "_",
      location_slug,
      "_",
      start_token,
      "_",
      end_token,
      ".csv"
    )
  )
}


#' Get days in a given month
#'
#' @param yy Integer year.
#' @param mm Integer month.
#' @return Character vector of day numbers, e.g. "01", "02".
#' @keywords internal
#' @noRd
days_in_month <- function(yy, mm) {
  start <- as.Date(sprintf("%04d-%02d-01", yy, mm))
  end <- seq(start, by = "month", length.out = 2)[2] - 1
  format(seq(start, end, by = "day"), "%d")
}


#' Parse a YYYY_MM year-month string
#'
#' @param x Character. Year-month string like "2024_05".
#' @param arg_name Character. Argument name used in error messages.
#' @return A list with `year` and `month`.
#' @keywords internal
#' @noRd
parse_ym <- function(x, arg_name = "ym") {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      !grepl("^\\d{4}_(0[1-9]|1[0-2])$", x)
  ) {
    stop(sprintf("`%s` must be a string like '2024_05'.", arg_name))
  }

  list(
    year = as.integer(substr(x, 1, 4)),
    month = as.integer(substr(x, 6, 7))
  )
}


#' Build one ERA5 download log row
#'
#' @param file Character. File name.
#' @param year Integer. Year.
#' @param month Integer. Month.
#' @param status Character. Download status.
#' @param bytes Numeric/integer. File size in bytes.
#' @param attempts Integer. Number of attempts.
#' @param path Character. Full output path.
#' @param request_url Character. CDS job/request URL.
#' @param error Character. Error text, if any.
#' @return A one-row data.frame.
#' @keywords internal
#' @noRd
make_era5_log_row <- function(
  file,
  year,
  month,
  status,
  bytes = NA_integer_,
  attempts = 0L,
  path,
  request_url = NA_character_,
  error = NA_character_
) {
  data.frame(
    file = file,
    year = year,
    month = month,
    status = status,
    bytes = bytes,
    attempts = attempts,
    path = path,
    request_url = request_url,
    error = error,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    stringsAsFactors = FALSE
  )
}