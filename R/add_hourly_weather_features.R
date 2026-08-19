#' Add hourly ERA5 weather features to model inputs
#'
#' Matches each observation to its nearest ERA5 cell and report hour, then
#' joins all available hourly weather columns.
#'
#' @param dataset Model-preparation data or a path to its RDS file.
#' @param dataset_type ERA5 dataset type.
#' @param data_dir Directory containing processed weather files.
#' @param write_output Whether to save the enriched dataset.
#' @param verbose Whether to print progress messages.
#'
#' @return The dataset with hourly weather features added.
#'
#' @export
add_hourly_weather_features <- function(
  dataset,
  dataset_type,
  data_dir = "data/proc",
  write_output = TRUE,
  verbose = TRUE
) {
  valid_datasets <- c(
    "reanalysis-era5-land",
    "reanalysis-era5-single-levels"
  )

  if (!dataset_type %in% valid_datasets) {
    stop(
      "`dataset_type` must be one of: ",
      paste(valid_datasets, collapse = ", "),
      call. = FALSE
    )
  }

  dataset_token <- if (
    dataset_type == "reanalysis-era5-land"
  ) {
    "land"
  } else {
    "single-levels"
  }

  # Load input data.
  if (is.character(dataset) && length(dataset) == 1L) {
    dataset_path <- dataset

    if (!file.exists(dataset_path)) {
      stop(
        "Dataset not found at ",
        dataset_path,
        call. = FALSE
      )
    }

    base_dataset <- readRDS(dataset_path)
  } else {
    base_dataset <- dataset
    dataset_path <- attr(
      base_dataset,
      "output_path"
    )
  }

  if (is.null(dataset_path)) {
    stop(
      "The dataset must have an `output_path` attribute.",
      call. = FALSE
    )
  }

  if (!all(c("lon", "lat") %in% names(base_dataset))) {
    stop(
      "The dataset must contain `lon` and `lat`.",
      call. = FALSE
    )
  }

  if (
    !"datetime" %in% names(base_dataset) &&
      !all(c("date", "hour") %in% names(base_dataset))
  ) {
    stop(
      "The dataset must contain `datetime` or both `date` and `hour`.",
      call. = FALSE
    )
  }

  # Determine location.
  location_slug <- attr(
    base_dataset,
    "location_slug"
  )

  if (is.null(location_slug)) {
    filename <- basename(dataset_path)

    match <- regmatches(
      filename,
      regexec(
        "^model_prep_(.+?)_base",
        filename
      )
    )[[1L]]

    if (length(match) >= 2L) {
      location_slug <- match[[2L]]
    }
  }

  if (is.null(location_slug)) {
    stop(
      "Could not determine the location slug.",
      call. = FALSE
    )
  }

  # Load hourly weather.
  weather_path <- file.path(
    data_dir,
    paste0(
      "weather_",
      location_slug,
      "_",
      dataset_token,
      "_hourly.Rds"
    )
  )

  if (!file.exists(weather_path)) {
    stop(
      "Hourly weather file not found at ",
      weather_path,
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message(
      "Reading hourly weather from: ",
      weather_path
    )
  }

  weather <- readRDS(weather_path)
  weather$time <- as.POSIXct(
    weather$time,
    tz = "UTC"
  )

  # Create report hour.
  report_hour <- rep(
    as.POSIXct(NA, tz = "UTC"),
    nrow(base_dataset)
  )

  if ("datetime" %in% names(base_dataset)) {
    report_hour <- lubridate::floor_date(
      as.POSIXct(
        base_dataset$datetime,
        tz = "UTC"
      ),
      unit = "hour"
    )
  }

  if (all(c("date", "hour") %in% names(base_dataset))) {
    date_hour <- as.POSIXct(
      as.Date(base_dataset$date),
      tz = "UTC"
    ) + as.integer(
      as.character(base_dataset$hour)
    ) * 3600

    report_hour[is.na(report_hour)] <-
      date_hour[is.na(report_hour)]
  }

  if (all(is.na(report_hour))) {
    stop(
      "Could not create report hours.",
      call. = FALSE
    )
  }

  # Match observations to the nearest ERA5 cell.
  weather_cells <- weather |>
    dplyr::distinct(.data$lon, .data$lat) |>
    dplyr::arrange(.data$lon, .data$lat)

  nearest_index <- FNN::get.knnx(
    data = as.matrix(
      dplyr::select(
        weather_cells,
        .data$lon,
        .data$lat
      )
    ),
    query = as.matrix(
      dplyr::select(
        base_dataset,
        .data$lon,
        .data$lat
      )
    ),
    k = 1
  )$nn.index[, 1L]

  enriched <- base_dataset |>
    dplyr::mutate(
      grid_lon = weather_cells$lon[nearest_index],
      grid_lat = weather_cells$lat[nearest_index],
      weather_time = report_hour
    )

  # The model data already contain their own date.
  weather <- weather |>
    dplyr::select(
      -dplyr::any_of("date")
    )

  # Join every hourly weather column.
  enriched <- enriched |>
    dplyr::left_join(
      weather,
      by = c(
        "grid_lon" = "lon",
        "grid_lat" = "lat",
        "weather_time" = "time"
      )
    )

  # Set output metadata.
  output_path <- file.path(
    data_dir,
    paste0(
      tools::file_path_sans_ext(
        basename(dataset_path)
      ),
      "_wx_hourly.Rds"
    )
  )

  attr(enriched, "location_slug") <- location_slug
  attr(enriched, "weather_resolution") <- "hourly"
  attr(enriched, "weather_sources") <- weather_path
  attr(enriched, "output_path") <- output_path

  if (isTRUE(write_output)) {
    saveRDS(
      enriched,
      output_path
    )

    if (isTRUE(verbose)) {
      message(
        "Hourly weather dataset written to: ",
        output_path
      )
    }
  }

  if (isTRUE(verbose)) {
    message(
      "Added hourly weather to ",
      nrow(enriched),
      " observations."
    )
  }

  enriched
}