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
#'   )
#' )
#' }
add_weather_features <- function(
    dataset,
    data_dir = "data/proc",
    write_output = TRUE,
    verbose = TRUE) {

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
    message("Adding weather features for slug ", location_slug)
  }

  if (!all(c("lon", "lat", "date") %in% names(base_dataset))) {
    stop("Base dataset must contain `lon`, `lat`, and `date` columns.", call. = FALSE)
  }

  weather_files <- list(
    daily = paste0("weather_", location_slug, "_cell_daily.rds"),
    lag7 = paste0("weather_", location_slug, "_cell_lags_7d.Rds"),
    lag14 = paste0("weather_", location_slug, "_cell_lags_14d.Rds"),
    lag30 = paste0("weather_", location_slug, "_cell_lags_30d.Rds"),
    lag21 = paste0("weather_", location_slug, "_cell_lags_21d_lag7.Rds"),
    pptlags = paste0("weather_", location_slug, "_cell_ppt_lags.Rds")
  )

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
  weather_tables <- lapply(weather_paths, readr::read_rds)

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

  if (nrow(wx_cells) == 0) {
    stop("Weather daily table does not contain lon/lat entries.", call. = FALSE)
  }

  reports_matrix <- as.matrix(dplyr::select(base_dataset, "lon", "lat"))
  weather_matrix <- as.matrix(dplyr::select(wx_cells, "lon", "lat"))

  nn_index <- FNN::get.knnx(weather_matrix, reports_matrix, k = 1)$nn.index[, 1]

  enriched <- base_dataset |>
    dplyr::mutate(
      grid_lon = wx_cells$lon[nn_index],
      grid_lat = wx_cells$lat[nn_index]
    )

  select_if_exists <- function(tbl, cols) {
    available <- intersect(cols, names(tbl))
    dplyr::select(tbl, dplyr::all_of(available))
  }

  enriched <- enriched |>
    dplyr::left_join(
      select_if_exists(wx_daily, c("lon", "lat", "date", "mwi", "maxTM", "meanPPT24H", "mwi_zeros_past_14d", "FH_zeros_past_14d")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) |>
    dplyr::left_join(
      select_if_exists(weather_tables$pptlags, c("lon", "lat", "date", "PPT_7d_8daysago")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) |>
    dplyr::left_join(
      select_if_exists(weather_tables$lag30, c("lon", "lat", "date", "FW30", "FH30", "FT30", "mwi30")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) |>
    dplyr::left_join(
      select_if_exists(weather_tables$lag14, c("lon", "lat", "date", "FW14", "FH14", "FT14", "mwi14")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) |>
    dplyr::left_join(
      select_if_exists(weather_tables$lag21, c("lon", "lat", "date", "FW21", "FH21", "FT21", "mwi21")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    )

  stem <- tools::file_path_sans_ext(basename(dataset_path))
  destination_filename <- paste0(stem, "_wx.Rds")
  destination_path <- file.path(data_dir, destination_filename)

  base_attrs <- attributes(base_dataset)
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched, nm) <- preserve[[nm]]
  }

  attr(enriched, "weather_sources") <- unlist(weather_paths, use.names = TRUE)
  attr(enriched, "output_path") <- destination_path
  attr(enriched, "location_slug") <- location_slug

  if (isTRUE(write_output)) {
    dir.create(dirname(destination_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(enriched, destination_path)
  }

  enriched
}



