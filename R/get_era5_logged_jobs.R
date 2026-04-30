#' Retry ERA5 downloads from a CDS job log
#'
#' Reads a log created by `get_era5_data()`, finds rows with saved CDS job URLs,
#' and attempts to transfer the existing CDS jobs once. The log is updated in place.
#'
#' @param iso3 Character. ISO3 country code.
#' @param admin_level Integer or NULL. GADM administrative level.
#' @param admin_name Character or NULL. GADM admin name.
#' @param start_ym Character. Starting year-month in "YYYY_MM" format.
#' @param end_ym Character. Ending year-month in "YYYY_MM" format.
#' @param output_dir Character or NULL. Directory where downloaded files are stored.
#' @param dataset Character. ERA5 dataset to use.
#' @param status_to_retry Character vector. Status values to retry.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A list with `summary`, and/or `files`, and `log_file`.
#' @importFrom ecmwfr wf_transfer
#' @export
get_era5_logged_jobs <- function(
  iso3,
  admin_level = NULL,
  admin_name = NULL,
  start_ym,
  end_ym,
  output_dir = NULL,
  dataset = "reanalysis-era5-single-levels",
  status_to_retry = c(
    "job_created_not_downloaded",
    "submitted_not_downloaded",
    "failed_transfer"
  ),
  verbose = TRUE
) {
  .say <- function(...) if (isTRUE(verbose)) message(sprintf(...))

  if (is.null(iso3) || length(iso3) != 1L || !nzchar(iso3)) {
    stop("`iso3` must be a non-empty character scalar.")
  }

  iso3 <- toupper(as.character(iso3))
  iso_fragment <- tolower(iso3)

  if (!is.null(admin_name) && !nzchar(admin_name)) {
    admin_name <- NULL
  }

  if (!is.null(admin_name)) {
    if (is.null(admin_level) || length(admin_level) != 1L || is.na(admin_level)) {
      stop("When `admin_name` is supplied, `admin_level` must be supplied.")
    }
  }

  if (is.null(output_dir) || !nzchar(output_dir)) {
    if (!is.null(admin_name)) {
      ids <- build_location_identifiers(iso3, admin_level, admin_name)
      output_dir <- file.path("data/weather/grib", ids$slug)
    } else {
      output_dir <- file.path("data/weather/grib", iso_fragment)
    }
  }

  output_dir <- path.expand(output_dir)

  log_file <- build_era5_log_path(
    output_dir = output_dir,
    iso3 = iso3,
    admin_level = admin_level,
    admin_name = admin_name,
    start_ym = start_ym,
    end_ym = end_ym,
    dataset = dataset
  )

  if (!file.exists(log_file)) {
    stop("Log file not found: ", log_file)
  }

  log_df <- utils::read.csv(log_file, stringsAsFactors = FALSE)

  required_cols <- c("file", "year", "month", "status", "path", "request_url")
  missing_cols <- setdiff(required_cols, names(log_df))

  if (length(missing_cols)) {
    stop(
      "Log file is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!"bytes" %in% names(log_df)) log_df$bytes <- NA_integer_
  if (!"attempts" %in% names(log_df)) log_df$attempts <- NA_integer_
  if (!"error" %in% names(log_df)) log_df$error <- NA_character_
  if (!"updated_at" %in% names(log_df)) log_df$updated_at <- NA_character_

  idx <- which(
    log_df$status %in% status_to_retry &
      !is.na(log_df$request_url) &
      nzchar(log_df$request_url)
  )

  if (!length(idx)) {
    .say("No rows to retry in log: %s", log_file)

    return(list(
      summary = list(
        total_rows = nrow(log_df),
        rows_checked = 0L,
        downloaded = 0L,
        failed = 0L,
        log_file = log_file
      ),
      files = log_df,
      log_file = log_file
    ))
  }

  downloaded <- 0L
  failed <- 0L

  for (i in idx) {
    filename <- log_df$file[i]
    filepath <- log_df$path[i]
    job_url <- log_df$request_url[i]

    if (file.exists(filepath)) {
      log_df$status[i] <- "downloaded"
      log_df$bytes[i] <- file.size(filepath)
      log_df$error[i] <- NA_character_
      log_df$updated_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      downloaded <- downloaded + 1L

      utils::write.csv(log_df, log_file, row.names = FALSE)
      next
    }

    dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)

    .say("Trying transfer for %s", filename)

    res <- try(
      ecmwfr::wf_transfer(
        url = job_url,
        path = dirname(filepath),
        filename = basename(filepath)
      ),
      silent = TRUE
    )

    if (!inherits(res, "try-error") && file.exists(filepath)) {
      log_df$status[i] <- "downloaded"
      log_df$bytes[i] <- file.size(filepath)
      log_df$error[i] <- NA_character_
      downloaded <- downloaded + 1L

      .say("Downloaded %s", filename)
    } else {
      last_error <- paste(capture.output(print(res)), collapse = "\n")

      log_df$status[i] <- "failed_transfer"
      log_df$bytes[i] <- NA_integer_
      log_df$error[i] <- last_error
      failed <- failed + 1L

      message(sprintf(
        paste0(
          "Transfer failed for %s.\n",
          "Raw error from CDS/ecmwfr:\n%s"
        ),
        filename,
        last_error
      ))
    }

    old_attempts <- suppressWarnings(as.integer(log_df$attempts[i]))
    if (is.na(old_attempts)) old_attempts <- 0L

    log_df$attempts[i] <- old_attempts + 1L
    log_df$updated_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

    utils::write.csv(log_df, log_file, row.names = FALSE)
  }

  summary <- list(
    total_rows = nrow(log_df),
    rows_checked = length(idx),
    downloaded = downloaded,
    failed = failed,
    log_file = log_file
  )

  summary
}