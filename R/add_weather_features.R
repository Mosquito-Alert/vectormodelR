#' Add ERA5 weather features to Mosquito Alert model inputs
#'
#' Starts from the model-preparation dataset produced by
#' `initialize_ma_dataset()`, attaches ERA5-based weather metrics, and writes a
#' new `model_prep_*_wx.Rds` file alongside the existing outputs.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when sourcing the weather and
#'   base model-prep files.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param data_dir Directory that holds the `model_prep_*` and
#'   `weather_*` RDS files. Defaults to `"data/proc"`.
#' @param base_variant Suffix of the base model-prep file to read. Defaults to
#'   `"base"` (looks for `model_prep_<slug>_base.Rds`).
#' @param output_variant Suffix used for the weather-enriched output file.
#'   Defaults to `"wx"` (writes `model_prep_<slug>_wx.Rds`).
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
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona"
#' )
#' }
add_weather_features <- function(
    iso3,
    admin_level,
    admin_name,
    data_dir = "data/proc",
    base_variant = "base",
    output_variant = "wx") {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- ids$slug

  base_filename <- paste0("model_prep_", location_slug, "_", base_variant, ".Rds")
  base_path <- file.path(data_dir, base_filename)
  if (!file.exists(base_path)) {
    stop("Base dataset not found at ", base_path, call. = FALSE)
  }

  base_dataset <- readRDS(base_path)
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
  wx_cells <- wx_daily %>%
    dplyr::distinct(.data$lon, .data$lat) %>%
    dplyr::arrange(.data$lon, .data$lat)

  if (nrow(wx_cells) == 0) {
    stop("Weather daily table does not contain lon/lat entries.", call. = FALSE)
  }

  reports_matrix <- as.matrix(dplyr::select(base_dataset, "lon", "lat"))
  weather_matrix <- as.matrix(dplyr::select(wx_cells, "lon", "lat"))

  nn_index <- FNN::get.knnx(weather_matrix, reports_matrix, k = 1)$nn.index[, 1]

  enriched <- base_dataset %>%
    dplyr::mutate(
      grid_lon = wx_cells$lon[nn_index],
      grid_lat = wx_cells$lat[nn_index]
    )

  select_if_exists <- function(tbl, cols) {
    available <- intersect(cols, names(tbl))
    dplyr::select(tbl, dplyr::all_of(available))
  }

  enriched <- enriched %>%
    dplyr::left_join(
      select_if_exists(wx_daily, c("lon", "lat", "date", "mwi", "maxTM", "meanPPT24H", "mwi_zeros_past_14d", "FH_zeros_past_14d")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) %>%
    dplyr::left_join(
      select_if_exists(weather_tables$pptlags, c("lon", "lat", "date", "PPT_7d_8daysago")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) %>%
    dplyr::left_join(
      select_if_exists(weather_tables$lag30, c("lon", "lat", "date", "FW30", "FH30", "FT30", "mwi30")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) %>%
    dplyr::left_join(
      select_if_exists(weather_tables$lag14, c("lon", "lat", "date", "FW14", "FH14", "FT14", "mwi14")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    ) %>%
    dplyr::left_join(
      select_if_exists(weather_tables$lag21, c("lon", "lat", "date", "FW21", "FH21", "FT21", "mwi21")),
      by = c("grid_lon" = "lon", "grid_lat" = "lat", "date" = "date")
    )

  destination_filename <- paste0("model_prep_", location_slug, "_", output_variant, ".Rds")
  destination_path <- file.path(data_dir, destination_filename)
  dir.create(dirname(destination_path), recursive = TRUE, showWarnings = FALSE)

  base_attrs <- attributes(base_dataset)
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched, nm) <- preserve[[nm]]
  }

  attr(enriched, "weather_sources") <- unlist(weather_paths, use.names = TRUE)
  attr(enriched, "output_path") <- destination_path

  saveRDS(enriched, destination_path)

  enriched
}



