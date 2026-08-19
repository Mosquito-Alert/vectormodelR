#' Add daily ERA5 weather features to model inputs
#'
#' Matches observations to the nearest ERA5 cell and adds every column from
#' the daily weather and precipitation-lag tables.
#'
#' @param dataset Model-preparation data or a path to its RDS file.
#' @param dataset_type Either `"reanalysis-era5-land"` or
#'   `"reanalysis-era5-single-levels"`.
#' @param data_dir Directory containing processed weather files.
#' @param write_output Whether to write the enriched dataset.
#' @param verbose Whether to print progress messages.
#'
#' @return The model-preparation dataset with daily weather features added.
#'
#' @export
add_daily_weather_features <- function(
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

  # Load the model-preparation dataset.
  if (
    is.character(dataset) &&
      length(dataset) == 1L
  ) {
    dataset_path <- dataset

    if (!file.exists(dataset_path)) {
      stop(
        "Dataset not found at ",
        dataset_path,
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Reading dataset from: ", dataset_path)
    }

    base_dataset <- readRDS(dataset_path)
  } else {
    base_dataset <- dataset

    dataset_path <- attr(
      base_dataset,
      "output_path",
      exact = TRUE
    )

    if (is.null(dataset_path)) {
      stop(
        "The supplied dataset must have an `output_path` attribute.",
        call. = FALSE
      )
    }
  }

  if (!is.data.frame(base_dataset)) {
    stop(
      "`dataset` must contain a data frame.",
      call. = FALSE
    )
  }

  required_cols <- c(
    "lon",
    "lat",
    "date"
  )

  missing_cols <- setdiff(
    required_cols,
    names(base_dataset)
  )

  if (length(missing_cols) > 0L) {
    stop(
      "Dataset is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Determine the location slug.
  location_slug <- attr(
    base_dataset,
    "location_slug",
    exact = TRUE
  )

  if (
    is.null(location_slug) ||
      !nzchar(location_slug)
  ) {
    filename <- basename(dataset_path)

    matched <- regmatches(
      filename,
      regexec(
        "^model_prep_(.+?)_base",
        filename
      )
    )[[1L]]

    if (length(matched) >= 2L) {
      location_slug <- matched[[2L]]
    }
  }

  if (
    is.null(location_slug) ||
      !nzchar(location_slug)
  ) {
    stop(
      "Could not determine the location slug.",
      call. = FALSE
    )
  }

  # Locate processed weather files.
  daily_path <- file.path(
    data_dir,
    paste0(
      "weather_",
      location_slug,
      "_",
      dataset_token,
      "_cell_daily.Rds"
    )
  )

  ppt_lag_path <- file.path(
    data_dir,
    paste0(
      "weather_",
      location_slug,
      "_",
      dataset_token,
      "_cell_ppt_lags.Rds"
    )
  )

  if (!file.exists(daily_path)) {
    stop(
      "Daily weather file not found at ",
      daily_path,
      call. = FALSE
    )
  }

  if (!file.exists(ppt_lag_path)) {
    stop(
      "Precipitation-lag file not found at ",
      ppt_lag_path,
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message("Adding daily weather for: ", location_slug)
    message("Daily weather file: ", daily_path)
    message("Precipitation-lag file: ", ppt_lag_path)
  }

  wx_daily <- readRDS(daily_path)
  ppt_lags <- readRDS(ppt_lag_path)

  base_dataset$date <- as.Date(base_dataset$date)
  wx_daily$date <- as.Date(wx_daily$date)
  ppt_lags$date <- as.Date(ppt_lags$date)

  weather_keys <- c(
    "lon",
    "lat",
    "date"
  )

  if (anyDuplicated(wx_daily[weather_keys])) {
    stop(
      "Daily weather contains duplicate lon-lat-date rows.",
      call. = FALSE
    )
  }

  if (anyDuplicated(ppt_lags[weather_keys])) {
    stop(
      "Precipitation lags contain duplicate lon-lat-date rows.",
      call. = FALSE
    )
  }

  # Find the nearest ERA5 cell for each observation.
  weather_cells <- wx_daily |>
    dplyr::distinct(
      .data$lon,
      .data$lat
    ) |>
    dplyr::arrange(
      .data$lon,
      .data$lat
    )

  report_coordinates <- as.matrix(
    base_dataset[c("lon", "lat")]
  )

  weather_coordinates <- as.matrix(
    weather_cells[c("lon", "lat")]
  )

  nearest_index <- FNN::get.knnx(
    data = weather_coordinates,
    query = report_coordinates,
    k = 1
  )$nn.index[, 1L]

  enriched <- base_dataset |>
    dplyr::mutate(
      grid_lon = weather_cells$lon[nearest_index],
      grid_lat = weather_cells$lat[nearest_index]
    )

  # Join every available weather column.
  enriched <- enriched |>
    dplyr::left_join(
      wx_daily,
      by = c(
        "grid_lon" = "lon",
        "grid_lat" = "lat",
        "date" = "date"
      )
    ) |>
    dplyr::left_join(
      ppt_lags,
      by = c(
        "grid_lon" = "lon",
        "grid_lat" = "lat",
        "date" = "date"
      )
    )

  # Preserve metadata from the input dataset.
  input_attributes <- attributes(base_dataset)

  preserve <- input_attributes[
    setdiff(
      names(input_attributes),
      c("names", "row.names", "class")
    )
  ]

  for (attribute_name in names(preserve)) {
    attr(enriched, attribute_name) <-
      preserve[[attribute_name]]
  }

  # Construct output path.
  input_stem <- tools::file_path_sans_ext(
    basename(dataset_path)
  )

  output_path <- file.path(
    data_dir,
    paste0(
      input_stem,
      "_wx.Rds"
    )
  )

  attr(enriched, "weather_sources") <- c(
    daily = daily_path,
    pptlags = ppt_lag_path
  )

  attr(enriched, "weather_resolution") <- "daily"
  attr(enriched, "location_slug") <- location_slug
  attr(enriched, "output_path") <- output_path

  if (isTRUE(write_output)) {
    dir.create(
      dirname(output_path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    saveRDS(
      enriched,
      output_path
    )

    if (isTRUE(verbose)) {
      message("Daily weather dataset written to: ", output_path)
    }
  }

  if (isTRUE(verbose)) {
    message(
      "Added ",
      ncol(wx_daily) - 3L,
      " daily weather features and ",
      ncol(ppt_lags) - 3L,
      " precipitation-lag features."
    )

    message(
      "Final dataset: ",
      nrow(enriched),
      " rows and ",
      ncol(enriched),
      " columns."
    )
  }

  enriched
}