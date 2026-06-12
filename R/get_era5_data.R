#' Download ERA5 climate data from the Copernicus Climate Data Store
#'
#' @param iso3 character. ISO3 country code (e.g., "BGD", "ESP", "USA").
#'   When `admin_level`/`admin_name` are also supplied, the download bbox is
#'   derived from the matching GADM unit.
#' @param admin_level integer. GADM administrative level
#'   (0=country, 1=region, 2=province, ...). Used only when `admin_name`
#'   is supplied.
#' @param admin_name character. Exact `NAME_<level>` value to select within
#'   GADM. When provided, `get_era5_data()` downloads a bbox around that
#'   admin unit.
#' @param bbox_margin_deg numeric. Margin (in degrees) added on each side of
#'   the derived bbox. Defaults to 0.5.
#' @param bounding_box numeric(4). c(north, west, south, east) in decimal
#'   degrees. Ignored if `iso3` is provided.
#' @param output_dir character. Directory where downloaded files will be
#'   saved. Default NULL uses `data/weather/grib/<iso3>` for country-wide
#'   downloads, or `data/weather/grib/<iso3>_<admin_level>_<admin_name>`
#'   when `admin_name` is provided.
#' @param dataset character. One of "reanalysis-era5-single-levels" or
#'   "reanalysis-era5-land".
#' @param variables character(). ERA5 variable short names. Default common
#'   surface vars.
#' @param start_ym character. Starting year-month in "YYYY_MM" format,
#'   e.g. "2024_05". Default = current year-month.
#' @param end_ym character. Ending year-month in "YYYY_MM" format,
#'   e.g. "2024_11". Default = current year-month.
#' @param ecmwfr_key character or NULL. If provided and `write_key = TRUE`,
#'   it will be saved via `wf_set_key()`. If NULL, will try
#'   `Sys.getenv("CDS_API_KEY")`. Otherwise requires a pre-existing keyring
#'   entry.
#' @param ecmwfr_user character. Keyring label to use with ecmwfr.
#'   Default "ecmwfr".
#' @param write_key logical. Persist `ecmwfr_key` to the keyring
#'   (off by default for public packages).
#' @param hours character(). Hours like "00:00"…"23:00". Default all 24 hours.
#' @param retry integer. Number of retries after the first attempt. Default 2.
#'   Retries are only used when no CDS job URL was created.
#' @param pause_between_requests_sec numeric. Seconds to wait after a
#'   successful download before sending the next request.
#' @param pause_sec numeric. Seconds to wait between failed attempts. Default 600.
#' @param verbose logical. Pass to `wf_request(verbose=)`. Default FALSE.
#' @param return_files logical. If TRUE, returns a list with `summary` and `files`, otherwise just `summary`.
#'
#' @return A list with `summary` and `files`. A CSV log is also written to
#'   `<output_dir>/logs/`.
#' @importFrom ecmwfr wf_request wf_set_key wf_get_key wf_datasets
#' @export
get_era5_data <- function(
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    bbox_margin_deg = 0.5,
    bounding_box = NULL,
    output_dir = NULL,
    dataset = "reanalysis-era5-single-levels",
    variables = c(
      "2m_dewpoint_temperature",
      "2m_temperature",
      "10m_u_component_of_wind",
      "10m_v_component_of_wind",
      "surface_pressure",
      "total_precipitation"
    ),
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
) {

  # ---- validate & normalize ----
  if (is.null(ecmwfr_key) || !nzchar(ecmwfr_key)) {
    stop(
      "ECMWFR_KEY is missing or empty. Please ensure it is defined in your ",
      ".Renviron file. You may need to restart your R session."
    )
  }

  valid_datasets <- c("reanalysis-era5-single-levels", "reanalysis-era5-land")
  if (!dataset %in% valid_datasets) {
    stop("`dataset` must be one of: ", paste(valid_datasets, collapse = ", "))
  }

  retry <- as.integer(retry)
  if (length(retry) != 1L || is.na(retry) || retry < 0L) {
    stop("`retry` must be a single non-negative integer.")
  }

  pause_sec <- as.numeric(pause_sec)
  if (length(pause_sec) != 1L || is.na(pause_sec) || pause_sec < 0) {
    stop("`pause_sec` must be a single non-negative number.")
  }

  pause_between_requests_sec <- as.numeric(pause_between_requests_sec)
  if (
    length(pause_between_requests_sec) != 1L ||
      is.na(pause_between_requests_sec) ||
      pause_between_requests_sec < 0
  ) {
    stop("`pause_between_requests_sec` must be a single non-negative number.")
  }

  if (!is.null(iso3) && nzchar(iso3)) {
    iso3 <- toupper(as.character(iso3))

    if (
      length(iso3) != 1L ||
        nchar(iso3) != 3L ||
        !grepl("^[A-Z]{3}$", iso3)
    ) {
      stop("`iso3` must be a single three-letter ISO3 code, e.g. 'ITA'.")
    }
  } else {
    iso3 <- NULL
  }

  if (!is.null(admin_name) && !nzchar(admin_name)) {
    admin_name <- NULL
  }

  if (is.null(admin_name)) {
    admin_level <- NULL
  } else {
    if (is.null(iso3)) {
      stop("When `admin_name` is supplied, you must also supply `iso3`.")
    }

    if (
      is.null(admin_level) ||
        length(admin_level) != 1L ||
        is.na(admin_level)
    ) {
      stop(
        "When `admin_name` is supplied, `admin_level` must be a single ",
        "non-missing value."
      )
    }

    admin_level <- as.integer(admin_level)

    if (
      !is.finite(bbox_margin_deg) ||
        length(bbox_margin_deg) != 1L ||
        bbox_margin_deg < 0
    ) {
      stop("`bbox_margin_deg` must be a single non-negative number.")
    }
  }

  if (is.null(iso3) && is.null(bounding_box)) {
    stop("Provide either `iso3` or `bounding_box`.")
  }

  # ---- resolve year-month range ----
  start_parsed <- parse_ym(start_ym, "start_ym")
  end_parsed <- parse_ym(end_ym, "end_ym")

  start_year <- start_parsed$year
  start_month <- start_parsed$month
  end_year <- end_parsed$year
  end_month <- end_parsed$month

  if (
    start_year > end_year ||
      (start_year == end_year && start_month > end_month)
  ) {
    stop("`start_ym` must be before or equal to `end_ym`.")
  }

  cy <- as.integer(format(Sys.Date(), "%Y"))
  cm <- as.integer(format(Sys.Date(), "%m"))

  if (
    start_year > cy ||
      (start_year == cy && start_month > cm)
  ) {
    stop("`start_ym` is in the future.")
  }

  if (
    end_year > cy ||
      (end_year == cy && end_month > cm)
  ) {
    end_year <- cy
    end_month <- cm
  }

  # ---- dataset exists? ----
  ds <- try(ecmwfr::wf_datasets(), silent = TRUE)

  if (!inherits(ds, "try-error")) {
    if (!dataset %in% ds$name) {
      stop(sprintf(
        "Dataset '%s' not found in wf_datasets(). Check spelling or update ecmwfr.",
        dataset
      ))
    }
  }

  # ---- resolve bounding box ----
  if (!is.null(admin_name)) {
    if (
      !requireNamespace("geodata", quietly = TRUE) ||
        !requireNamespace("sf", quietly = TRUE)
    ) {
      stop(
        "Packages {geodata} and {sf} are required to derive an admin-unit bbox. ",
        "Install them or provide `bounding_box`."
      )
    }

    g <- geodata::gadm(
      country = iso3,
      level = admin_level,
      path = file.path("data/proc", "gadm")
    )

    g <- sf::st_as_sf(g)
    nmcol <- paste0("NAME_", admin_level)

    if (!nmcol %in% names(g)) {
      stop(
        "GADM geometry is missing expected name column ", nmcol,
        " for level ", admin_level, "."
      )
    }

    g <- g[g[[nmcol]] == admin_name, , drop = FALSE]

    if (nrow(g) == 0) {
      stop(
        "Admin name '", admin_name, "' not found at level ",
        admin_level, " for ", iso3, "."
      )
    }

    bb <- sf::st_bbox(sf::st_union(sf::st_make_valid(g)))

    bounding_box <- c(
      north = as.numeric(bb[["ymax"]]) + bbox_margin_deg,
      west = as.numeric(bb[["xmin"]]) - bbox_margin_deg,
      south = as.numeric(bb[["ymin"]]) - bbox_margin_deg,
      east = as.numeric(bb[["xmax"]]) + bbox_margin_deg
    )

    message(sprintf(
      paste0(
        "Using GADM bbox for %s level %d '%s' (margin %.3f°): ",
        "[N=%.4f, W=%.4f, S=%.4f, E=%.4f]"
      ),
      iso3,
      admin_level,
      admin_name,
      bbox_margin_deg,
      bounding_box[1],
      bounding_box[2],
      bounding_box[3],
      bounding_box[4]
    ))
  } else if (!is.null(iso3)) {
    if (exists("get_bounding_boxes", mode = "function")) {
      bbox_result <- get_bounding_boxes(countries = iso3, format = "vector")

      if (is.null(bbox_result) || length(bbox_result) == 0) {
        stop(sprintf("Country code '%s' not found by get_bounding_boxes().", iso3))
      }

      bounding_box <- bbox_result[[1]]
    } else {
      if (
        !requireNamespace("rnaturalearth", quietly = TRUE) ||
          !requireNamespace("sf", quietly = TRUE)
      ) {
        stop(
          "Provide `bounding_box` or install {rnaturalearth} and {sf} ",
          "to look up ISO3 bounding boxes."
        )
      }

      world <- rnaturalearth::ne_countries(
        scale = "medium",
        returnclass = "sf"
      )

      row <- world[world$iso_a3 == iso3, ]

      if (nrow(row) == 0) {
        stop(sprintf("ISO3 '%s' not found in Natural Earth.", iso3))
      }

      bb <- sf::st_bbox(row$geometry)

      bounding_box <- c(
        north = as.numeric(bb["ymax"]),
        west = as.numeric(bb["xmin"]),
        south = as.numeric(bb["ymin"]),
        east = as.numeric(bb["xmax"])
      )
    }

    message(sprintf(
      "Using bounding box for %s: [N=%.4f, W=%.4f, S=%.4f, E=%.4f]",
      iso3,
      bounding_box[1],
      bounding_box[2],
      bounding_box[3],
      bounding_box[4]
    ))
  } else {
    if (length(bounding_box) != 4 || !is.numeric(bounding_box)) {
      stop("`bounding_box` must be numeric(4): c(north, west, south, east).")
    }
  }

  area_str <- paste(as.numeric(bounding_box), collapse = "/")

  iso_fragment <- if (!is.null(iso3)) {
    tolower(iso3)
  } else {
    "bbox"
  }

  admin_fragment <- NULL

  if (!is.null(admin_name)) {
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    admin_fragment <- paste0(ids$admin_level, "_", ids$admin_name)
  }

  # ---- auth ----
  if (!is.null(ecmwfr_key) && isTRUE(write_key)) {
    ecmwfr::wf_set_key(key = ecmwfr_key, user = ecmwfr_user)
  }

  if (requireNamespace("keyring", quietly = TRUE)) {
    if (isTRUE(try(keyring::keyring_is_locked(), silent = TRUE))) {
      try(keyring::keyring_unlock(), silent = TRUE)
    }
  }

  if (is.null(ecmwfr::wf_get_key(ecmwfr_user))) {
    stop(
      sprintf("No CDS token found for user '%s'. ", ecmwfr_user),
      "Set it once with ecmwfr::wf_set_key(key = '<YOUR_TOKEN>', user = '",
      ecmwfr_user,
      "') or call get_era5_data(write_key = TRUE)."
    )
  }

  # ---- paths ----
  if (is.null(output_dir) || !nzchar(output_dir)) {
    if (!is.null(admin_name)) {
      ids <- build_location_identifiers(iso3, admin_level, admin_name)
      output_dir <- file.path("data/weather/grib", ids$slug)
    } else {
      output_dir <- file.path("data/weather/grib", iso_fragment)
    }
  }

  output_dir <- path.expand(output_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  log_file <- build_era5_log_path(
    output_dir = output_dir,
    iso3 = if (!is.null(iso3)) iso3 else iso_fragment,
    admin_level = admin_level,
    admin_name = admin_name,
    start_ym = start_ym,
    end_ym = end_ym,
    dataset = dataset
  )

  requires_product_type <- identical(dataset, "reanalysis-era5-single-levels")

  ext <- if (dataset == "reanalysis-era5-land") {
    "zip"
  } else {
    "grib"
  }

  base_prefix <- if (dataset == "reanalysis-era5-land") {
    "era5land"
  } else {
    "era5"
  }

  total <- existed <- downloaded <- submitted_not_downloaded <- failed <- 0L
  rows <- list()

  message(sprintf(
    "Starting ERA5 downloads %04d_%02d–%04d_%02d (%s)",
    start_year,
    start_month,
    end_year,
    end_month,
    dataset
  ))

  # ---- main loop ----
  for (yy in start_year:end_year) {
    months <- 1:12

    if (yy == start_year) {
      months <- months[months >= start_month]
    }

    if (yy == end_year) {
      months <- months[months <= end_month]
    }

    if (yy == cy) {
      months <- months[months <= cm]
    }

    if (!length(months)) next

    for (mm in months) {
      mm_str <- sprintf("%02d", mm)
      day_vec <- days_in_month(yy, mm)

      file_prefix <- if (!is.null(admin_fragment)) {
        sprintf("%s_%s_%s", base_prefix, iso_fragment, admin_fragment)
      } else {
        sprintf("%s_%s", base_prefix, iso_fragment)
      }

      filename <- sprintf("%s_%d_%s.%s", file_prefix, yy, mm_str, ext)
      filepath <- file.path(output_dir, filename)

      total <- total + 1L

      if (file.exists(filepath)) {
        existed <- existed + 1L

        rows[[length(rows) + 1L]] <- make_era5_log_row(
          file = filename,
          year = yy,
          month = mm,
          status = "exists",
          bytes = file.size(filepath),
          attempts = 0L,
          path = filepath
        )

        utils::write.csv(do.call(rbind, rows), log_file, row.names = FALSE)
        next
      }

      req <- list(
        dataset_short_name = dataset,
        variable = variables,
        year = as.character(yy),
        month = mm_str,
        day = day_vec,
        time = hours,
        area = area_str,
        data_format = "grib",
        target = filename
      )

      if (requires_product_type) {
        req$product_type <- "reanalysis"
      }

      attempt <- 0L
      ok <- FALSE
      request_url <- NA_character_
      last_error <- NA_character_
      request_submitted_but_not_downloaded <- FALSE
      max_attempts <- retry + 1L

      repeat {
        attempt <- attempt + 1L

        res <- try(
          ecmwfr::wf_request(
            user = ecmwfr_user,
            request = req,
            transfer = TRUE,
            path = output_dir,
            time_out = 3600,
            verbose = verbose
          ),
          silent = TRUE
        )

        if (!inherits(res, "try-error")) {
          res_text <- paste(capture.output(print(res)), collapse = "\n")
          job_url <- extract_cds_job_url(res_text)

          if (!is.na(job_url)) {
            request_url <- job_url
          }

          if (file.exists(filepath)) {
            ok <- TRUE
            break
          }

          if (!is.na(job_url)) {
            last_error <- res_text
            request_submitted_but_not_downloaded <- TRUE

            message(sprintf(
              paste0(
                "CDS created a job record for %s, but transfer/polling did not complete.\n",
                "Saved job URL to log for later retry: %s"
              ),
              filename,
              log_file
            ))

            break
          }

          last_error <- res_text
        }

        err_text <- paste(capture.output(print(res)), collapse = "\n")
        last_error <- err_text
        job_url <- extract_cds_job_url(err_text)

        message(sprintf(
          paste0(
            "Request failed for %s on attempt %d of %d.\n",
            "Details saved to log: %s"
          ),
          filename,
          attempt,
          max_attempts,
          log_file
        ))

        if (!is.na(job_url)) {
          request_url <- job_url
          request_submitted_but_not_downloaded <- TRUE

        message(sprintf(
          paste0(
            "CDS created a job record for %s, but transfer/polling did not complete.\n",
            "Saved job URL to log for later retry: %s"
          ),
          filename,
          log_file
        ))

          break
        }

        if (attempt >= max_attempts) {
          message(sprintf("Giving up on %s after %d attempts.", filename, attempt))
          break
        }

        message(sprintf(
          "Waiting %.0f seconds before retrying %s.",
          pause_sec,
          filename
        ))

        Sys.sleep(pause_sec)
      }

      file_ok <- file.exists(filepath)

      if (file_ok) {
        downloaded <- downloaded + 1L

        rows[[length(rows) + 1L]] <- make_era5_log_row(
          file = filename,
          year = yy,
          month = mm,
          status = if (ok) "downloaded" else "downloaded_with_warning",
          bytes = file.size(filepath),
          attempts = attempt,
          path = filepath,
          request_url = request_url,
          error = NA_character_
        )

        utils::write.csv(do.call(rbind, rows), log_file, row.names = FALSE)
        Sys.sleep(pause_between_requests_sec)

      } else if (isTRUE(request_submitted_but_not_downloaded)) {
        submitted_not_downloaded <- submitted_not_downloaded + 1L

        rows[[length(rows) + 1L]] <- make_era5_log_row(
          file = filename,
          year = yy,
          month = mm,
          status = "submitted_not_downloaded",
          bytes = NA_integer_,
          attempts = attempt,
          path = filepath,
          request_url = request_url,
          error = last_error
        )

        utils::write.csv(do.call(rbind, rows), log_file, row.names = FALSE)

      } else {
        failed <- failed + 1L

        rows[[length(rows) + 1L]] <- make_era5_log_row(
          file = filename,
          year = yy,
          month = mm,
          status = "failed",
          bytes = NA_integer_,
          attempts = attempt,
          path = filepath,
          request_url = NA_character_,
          error = last_error
        )

        utils::write.csv(do.call(rbind, rows), log_file, row.names = FALSE)
      }
    }
  }

  files_df <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      file = character(),
      year = integer(),
      month = integer(),
      status = character(),
      bytes = integer(),
      attempts = integer(),
      path = character(),
      request_url = character(),
      error = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    )
  }

  utils::write.csv(files_df, log_file, row.names = FALSE)

  summary <- list(
    total_files_needed = total,
    files_already_exist = existed,
    files_downloaded = downloaded,
    files_submitted_not_downloaded = submitted_not_downloaded,
    files_failed = failed,
    success_rate = if (total > 0) {
      (existed + downloaded) / total * 100
    } else {
      NA_real_
    },
    output_directory = output_dir,
    log_file = log_file
  )

  if (isTRUE(return_files)) {
    list(summary = summary, files = files_df)
  } else {
    summary
  }
}