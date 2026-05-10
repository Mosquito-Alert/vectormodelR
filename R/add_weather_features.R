#' Add ERA5 weather features to Mosquito Alert model inputs
#'
#' Starts from the model-preparation dataset produced by
#' `initialize_ma_dataset()`, attaches ERA5-based weather metrics, and writes a
#' new `model_prep_*_wx.Rds` file alongside the existing outputs.
#'
#' @param dataset Either the in-memory base model-prep dataset (as returned by
#'   [initialize_ma_dataset()] or subsequent enrichment helpers) or a path to
#'   the corresponding RDS file. When a data object is supplied it must carry an
#'   `output_path` attribute naming the last saved file; the enriched dataset is
#'   written to `data_dir` with `_wx.Rds` appended to that stem when
#'   `write_output` is `TRUE`.
#' @param dataset_type Character. ERA5 dataset: "reanalysis-era5-single-levels" 
#'   or "reanalysis-era5-land". Must match the dataset used when processing weather
#'   data with `process_era5_data()`.
#' @param weather_resolution Character. Weather feature resolution to add.
#'   Use `"daily"` for the existing daily summaries and precipitation lag
#'   windows, or `"hourly"` to join the processed hourly ERA5 cell table by
#'   nearest ERA5 cell and report hour. Hourly joins use `datetime` when
#'   available and fall back to `date` plus `hour` for rows with missing
#'   `datetime`.
#' @param data_dir Directory that holds the weather RDS files and where the
#'   weather-enriched dataset will be written. Defaults to `"data/proc"`.
#' @param write_output Logical flag; when `TRUE` (default) the enriched dataset
#'   is written to disk. Set to `FALSE` to skip writing while still returning the
#'   augmented object and updating its metadata.
#' @param verbose Logical; if `TRUE`, prints status messages.
#'
#' @return A tibble containing the enriched dataset. Attributes from the base
#'   dataset are preserved, and additional attributes (`weather_sources` and
#'   `output_path`) identify the files that were read and written.
#' @export
#' @importFrom rlang .data
#' @importFrom FNN get.knnx
#' @examples
#' \dontrun{
#' add_weather_features(
#'   dataset = initialize_ma_dataset(
#'     iso3 = "ESP",
#'     admin_level = 4,
#'     admin_name = "Barcelona"
#'   ),
#'   dataset_type = "reanalysis-era5-land"
#' )
#' }
add_weather_features <- function(
    dataset,
    dataset_type,
    weather_resolution = c("daily", "hourly"),
    data_dir = "data/proc",
    write_output = TRUE,
    verbose = TRUE) {

  weather_resolution <- match.arg(weather_resolution)

  # Validate dataset_type
  if (is.null(dataset_type) || !nzchar(dataset_type)) {
    stop("`dataset_type` is required. Use 'reanalysis-era5-single-levels' or 'reanalysis-era5-land'.", call. = FALSE)
  }
  valid_datasets <- c("reanalysis-era5-single-levels", "reanalysis-era5-land")
  if (!dataset_type %in% valid_datasets) {
    stop("`dataset_type` must be one of: ", paste(valid_datasets, collapse = ", "), call. = FALSE)
  }
  
  # Determine dataset token for file naming
  dataset_token <- if (dataset_type == "reanalysis-era5-land") "land" else "single-levels"

  dataset_is_path <- is.character(dataset) && length(dataset) == 1L
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) {
      stop("Base dataset not found at ", dataset_path, call. = FALSE)
    }
    base_dataset <- readRDS(dataset_path)
  } else {
    base_dataset <- dataset
    dataset_path <- attr(base_dataset, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop(
        "Input dataset must carry an `output_path` attribute or be provided as a file path.",
        call. = FALSE
      )
    }
  }

  infer_slug <- function(path) {
    fname <- basename(path)
    matches <- regexec("^model_prep_(.+?)_base", fname)
    parts <- regmatches(fname, matches)[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }

  location_slug <- attr(base_dataset, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) {
    location_slug <- infer_slug(dataset_path)
  }
  if (is.na(location_slug) || !nzchar(location_slug)) {
    stop("Could not determine location slug from dataset; ensure it carries a `location_slug` attribute.", call. = FALSE)
  }
  attr(base_dataset, "location_slug") <- location_slug

  if (isTRUE(verbose)) {
    message("Adding ", weather_resolution, " weather features for slug ", location_slug)
  }

  required_base_cols <- c("lon", "lat")
  if (identical(weather_resolution, "daily")) {
    required_base_cols <- c(required_base_cols, "date")
  }
  missing_base_cols <- setdiff(required_base_cols, names(base_dataset))
  if (length(missing_base_cols)) {
    stop(
      "Base dataset is missing required column(s): ",
      paste(missing_base_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (identical(weather_resolution, "hourly") &&
      !"datetime" %in% names(base_dataset) &&
      !all(c("date", "hour") %in% names(base_dataset))) {
    stop(
      "Hourly weather requires a `datetime` column or both `date` and `hour` columns.",
      call. = FALSE
    )
  }

  weather_files <- if (identical(weather_resolution, "daily")) {
    list(
      daily = paste0("weather_", location_slug, "_", dataset_token, "_cell_daily.Rds"),
      pptlags = paste0("weather_", location_slug, "_", dataset_token, "_cell_ppt_lags.Rds")
    )
  } else {
    list(
      hourly = paste0("weather_", location_slug, "_", dataset_token, "_hourly.Rds")
    )
  }

  resolve_weather_path <- function(filename) {
    candidates <- c(
      file.path(data_dir, filename),
      file.path(data_dir, gsub("\\.rds$", ".Rds", filename, ignore.case = TRUE)),
      file.path(data_dir, gsub("\\.Rds$", ".rds", filename, ignore.case = TRUE))
    )
    existing <- candidates[file.exists(candidates)]
    if (length(existing) == 0) {
      stop("Weather file not found (checked): ", paste(candidates, collapse = ", "), call. = FALSE)
    }
    existing[[1]]
  }

  weather_paths <- lapply(weather_files, resolve_weather_path)

  if (isTRUE(verbose)) {
    message("Reading ERA5 weather tables from:")
    for (nm in names(weather_paths)) {
      message("  • ", nm, ": ", weather_paths[[nm]])
    }
  }
  weather_tables <- lapply(weather_paths, readr::read_rds)

  select_required <- function(tbl, cols, source_name) {
    missing <- setdiff(cols, names(tbl))

    if (length(missing)) {
      stop(
        "Weather table '", source_name, "' is missing expected columns: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }

    dplyr::select(tbl, dplyr::all_of(cols))
  }

  floor_to_hour <- function(x) {
    parsed <- if (inherits(x, "POSIXt")) {
      as.POSIXct(x, tz = "UTC")
    } else {
      suppressWarnings(as.POSIXct(
        x,
        tz = "UTC",
        tryFormats = c(
          "%Y-%m-%d %H:%M:%OS",
          "%Y-%m-%d %H:%M:%S",
          "%Y-%m-%d %H:%M",
          "%Y-%m-%d"
        )
      ))
    }

    as.POSIXct(
      floor(as.numeric(parsed) / 3600) * 3600,
      origin = "1970-01-01",
      tz = "UTC"
    )
  }

  build_report_hour <- function(tbl) {
    from_datetime <- rep(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"), nrow(tbl))
    if ("datetime" %in% names(tbl)) {
      from_datetime <- floor_to_hour(tbl[["datetime"]])
    }

    from_date_hour <- rep(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"), nrow(tbl))
    if (all(c("date", "hour") %in% names(tbl))) {
      report_dates <- as.Date(tbl[["date"]])
      report_hours <- suppressWarnings(as.integer(tbl[["hour"]]))
      valid <- !is.na(report_dates) & !is.na(report_hours)
      from_date_hour[valid] <- as.POSIXct(report_dates[valid], tz = "UTC") + report_hours[valid] * 3600
    }

    out <- from_datetime
    missing_datetime <- is.na(out)
    out[missing_datetime] <- from_date_hour[missing_datetime]
    out
  }

  if (identical(weather_resolution, "daily")) {
    weather_tables <- lapply(weather_tables, function(tbl) {
      if (!"date" %in% names(tbl)) {
        stop("Weather table is missing a `date` column.", call. = FALSE)
      }
      dplyr::mutate(tbl, date = as.Date(.data$date))
    })

    wx_daily <- weather_tables$daily
    wx_cells <- wx_daily |>
      dplyr::distinct(.data$lon, .data$lat) |>
      dplyr::arrange(.data$lon, .data$lat)
  } else {
    wx_hourly <- weather_tables$hourly
    select_required(
      wx_hourly,
      c("lon", "lat", "time", "t2m_C_hour", "RH_hour", "ws10_hour", "ppt_mm_hour", "ppt_mm_prev_6h", "ppt_mm_prev_24h", "t2m_C_mean_prev_6h", "RH_mean_prev_6h"),
      "hourly"
    )
    wx_hourly$time <- floor_to_hour(wx_hourly[["time"]])

    wx_cells <- wx_hourly |>
      dplyr::distinct(.data$lon, .data$lat) |>
      dplyr::arrange(.data$lon, .data$lat)
  }

  if (nrow(wx_cells) == 0) {
    stop("Weather table does not contain lon/lat entries.", call. = FALSE)
  }

  reports_matrix <- as.matrix(dplyr::select(base_dataset, "lon", "lat"))
  weather_matrix <- as.matrix(dplyr::select(wx_cells, "lon", "lat"))

  nn_index <- FNN::get.knnx(weather_matrix, reports_matrix, k = 1)$nn.index[, 1]

  enriched <- base_dataset |>
    dplyr::mutate(
      grid_lon = wx_cells$lon[nn_index],
      grid_lat = wx_cells$lat[nn_index]
    )

  if (identical(weather_resolution, "daily")) {
    enriched <- enriched |>
      dplyr::left_join(
        select_required(wx_daily, c("lon", "lat", "date", "mwi", "maxTM", "meanPPT24H", "mwi_zeros_past_14d", "FH_zeros_past_14d"), "daily"),
        by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
      ) |>
      dplyr::left_join(
        select_required(weather_tables$pptlags, c("lon", "lat", "date", "PPT_3d", "PPT_7d", "PPT_14d", "PPT_21d", "PPT_30d", "PPT_3d_lag7", "PPT_7d_lag7", "PPT_14d_lag7", "PPT_21d_lag7", "PPT_30d_lag7"), "pptlags"),
        by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
      )
  } else {
    report_hour <- build_report_hour(enriched)
    if (!any(!is.na(report_hour))) {
      stop(
        "Hourly weather could not be joined because all report datetimes/hours are missing.",
        call. = FALSE
      )
    }

    hourly_cols <- c(
      "lon", "lat", "time",
      "t2m_C_hour", "RH_hour", "ws10_hour", "ppt_mm_hour",
      "ppt_mm_prev_6h", "ppt_mm_prev_24h",
      "t2m_C_mean_prev_6h", "RH_mean_prev_6h"
    )

    enriched <- enriched |>
      dplyr::mutate(weather_time = report_hour) |>
      dplyr::left_join(
        select_required(wx_hourly, hourly_cols, "hourly"),
        by = c("grid_lon" = "lon", "grid_lat" = "lat", "weather_time" = "time")
      )
  }

  stem <- tools::file_path_sans_ext(basename(dataset_path))
  destination_suffix <- if (identical(weather_resolution, "daily")) "_wx.Rds" else "_wx_hourly.Rds"
  destination_filename <- paste0(stem, destination_suffix)
  destination_path <- file.path(data_dir, destination_filename)

  base_attrs <- attributes(base_dataset)
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched, nm) <- preserve[[nm]]
  }

  attr(enriched, "weather_sources") <- unlist(weather_paths, use.names = TRUE)
  attr(enriched, "weather_resolution") <- weather_resolution
  attr(enriched, "output_path") <- destination_path
  attr(enriched, "location_slug") <- location_slug

  if (isTRUE(write_output)) {
    dir.create(dirname(destination_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(enriched, destination_path)
  }

  enriched
}



