#' Prepare Mosquito Alert reports for modelling
#'
#' @param iso3 Character scalar with the three-letter ISO3 country code.
#' @param admin_level Administrative level used when the hex grid was created.
#'   Accepts character or numeric input.
#' @param admin_name Name of the administrative unit used when the hex grid was
#'   created. The name is normalised (lowercase, non-alphanumerics replaced with
#'   underscores) before constructing the file path.
#' @param reports_path Path to the cleaned Mosquito Alert reports RDS file.
#' @param hex_grid_dir Directory containing pre-computed hex grid RDS files.
#' @param sampling_effort_url Optional URL pointing to the sampling effort CSV
#'   resource. Set to `NULL` to skip downloading.
#' @param android_start_date Optional date (character or `Date`) used to filter
#'   reports collected before the Android application launch.
#' @param output_dir Directory where the prepared dataset should be written.
#'   Defaults to `hex_grid_dir`.
#' @param dataset_variant Optional suffix inserted into the saved filename (for
#'   example "base" or "extended").
#'
#' @return A tibble of filtered reports. The object includes attributes
#'   `sampling_effort`, `hex_grid`, and `output_path` for downstream reuse.
#' @export
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' initialize_ma_dataset(
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona"
#' )
#' }
initialize_ma_dataset <- function(
    iso3,
    admin_level,
    admin_name,
    reports_path = "data/proc/mosquito_alert_cleaned_reports.Rds",
    hex_grid_dir = "data/proc",
    sampling_effort_url = "https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz",
    android_start_date = NULL,
    output_dir = NULL,
    dataset_variant = "base") {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  message("Preparing Mosquito Alert dataset for ", ids$slug, " …")

  grid_filename <- paste0("spatial_", ids$slug, "_hex_grid.rds")

  grid_path <- file.path(hex_grid_dir, grid_filename)
  if (!file.exists(grid_path)) {
    stop("Hex grid not found at ", grid_path, call. = FALSE)
  }

  message("• Loading hex grid: ", grid_path)
  hex_grid <- readr::read_rds(grid_path)
  if (!"grid_id" %in% names(hex_grid)) {
    stop("`grid_id` column not found in hex grid at ", grid_path, call. = FALSE)
  }

  sampling_effort <- NULL
  if (!is.null(sampling_effort_url)) {
    message("• Downloading sampling effort from remote source")
    sampling_effort <- readr::read_csv(sampling_effort_url, show_col_types = FALSE)
  }

  if (!file.exists(reports_path)) {
    stop("Reports file not found at ", reports_path, call. = FALSE)
  }
  message("• Loading Mosquito Alert reports: ", reports_path)
  reports <- readRDS(reports_path)
  reports <- reports %>%
    dplyr::filter(.data$report_type == "adult") %>%
    dplyr::mutate(
      date = as.Date(.data$date),
      ..rowid = dplyr::row_number()
    ) %>%
    dplyr::filter(!is.na(.data$lon), !is.na(.data$lat))

  datasets <- reports %>%
    dplyr::mutate(
      year = lubridate::year(.data$date),
      sea_days = lubridate::yday(.data$date),
      TigacellID_small = make_samplingcell_ids(.data$lon, .data$lat, 0.025)
    )

  if (!is.null(android_start_date)) {
    datasets <- datasets %>%
      dplyr::filter(.data$date > lubridate::as_date(android_start_date))
  }

  datasets <- datasets %>%
    dplyr::left_join(
      reports %>%
        dplyr::group_by(.data$user) %>%
        dplyr::summarise(
          mean_score = mean(.data$movelab_certainty_category, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "user"
    ) %>%
    dplyr::mutate(
      reliable_report = (.data$movelab_certainty_category > 0) |
        ((is.na(.data$movelab_certainty_category) | .data$movelab_certainty_category >= 0) &
           (.data$mean_score > 0))
    ) %>%
    dplyr::filter(.data$reliable_report) %>%
    dplyr::select(-"reliable_report") %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    sf::st_join(hex_grid["grid_id"], join = sf::st_within) %>%
    dplyr::filter(!is.na(.data$grid_id)) %>%
    sf::st_drop_geometry()

  message("• Writing primary dataset to RDS")
  destination_dir <- if (is.null(output_dir)) hex_grid_dir else output_dir
  variant_slug <- tolower(dataset_variant)
  variant_slug <- gsub("[^a-z0-9]+", "_", variant_slug)
  variant_slug <- gsub("^_+|_+$", "", variant_slug)
  if (variant_slug == "") {
    variant_slug <- "base"
  }

  dataset_filename <- paste0("model_prep_", ids$slug, "_", variant_slug, ".Rds")
  output_path <- file.path(destination_dir, dataset_filename)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(datasets, output_path)

  # Derive a common filename stem for any auxiliary artefacts we persist.
  output_stem <- file.path(destination_dir, paste0("model_prep_", ids$slug, "_"))

  # ---- Summaries of certainty-2 season bounds (overall and yearly) ----
  season_subset <- datasets %>%
    dplyr::filter(.data$movelab_certainty_category == 2)

  if (nrow(season_subset) > 0) {
    message("• Summarising certainty-2 season bounds")
    season_bounds <- season_subset %>%
      dplyr::summarise(
        season_start = suppressWarnings(min(.data$sea_days, na.rm = TRUE)),
        season_end = suppressWarnings(max(.data$sea_days, na.rm = TRUE)),
        .groups = "drop"
      )
    readr::write_rds(season_bounds, paste0(output_stem, "season_bounds.Rds"))

    season_bounds_yearly <- season_subset %>%
      dplyr::mutate(year = as.factor(.data$year)) %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(
        season_start = suppressWarnings(min(.data$sea_days, na.rm = TRUE)),
        season_end = suppressWarnings(max(.data$sea_days, na.rm = TRUE)),
        .groups = "drop"
      )
    readr::write_rds(season_bounds_yearly, paste0(output_stem, "season_bounds_yearly.Rds"))
  } else {
    season_bounds <- tibble::tibble(season_start = NA_real_, season_end = NA_real_)
    season_bounds_yearly <- tibble::tibble(year = factor(), season_start = numeric(), season_end = numeric())
    readr::write_rds(season_bounds, paste0(output_stem, "season_bounds.Rds"))
    readr::write_rds(season_bounds_yearly, paste0(output_stem, "season_bounds_yearly.Rds"))
  }

  # ---- Clip sampling effort to the admin geometry and capture min SE ----
  trs_daily <- NULL
  min_SE_logit <- NA_real_
  d_se <- NULL

  if (!is.null(sampling_effort) && nrow(sampling_effort) > 0 &&
      all(c("masked_lon", "masked_lat") %in% names(sampling_effort))) {
    message("• Clipping sampling effort to admin boundary")
    sampling_effort_sf <- tryCatch(
      sf::st_as_sf(
        sampling_effort,
        coords = c("masked_lon", "masked_lat"),
        crs = 4326,
        remove = FALSE
      ),
      error = function(e) NULL
    )

    if (!is.null(sampling_effort_sf)) {
      candidate_maps <- file.path(
        hex_grid_dir,
        c(
          paste0("spatial_", ids$slug, "_adm.Rds"),
          paste0("map_", ids$slug, "_adm.Rds")
        )
      )
      map_path <- candidate_maps[file.exists(candidate_maps)][1]

      if (!is.na(map_path)) {
        admin_map <- readr::read_rds(map_path) %>%
          sf::st_transform(4326)
        trs_daily <- sampling_effort_sf[admin_map, ]

        if (nrow(trs_daily) > 0) {
          readr::write_rds(trs_daily, paste0(output_stem, "trs_daily.Rds"))

          if ("SE" %in% names(trs_daily)) {
            positive_se <- trs_daily$SE[trs_daily$SE > 0 & trs_daily$SE < 1]
            if (length(positive_se) > 0) {
              min_SE <- min(positive_se, na.rm = TRUE)
              min_SE_logit <- log(min_SE / (1 - min_SE))
              readr::write_rds(min_SE_logit, paste0(output_stem, "min_SE_logit.Rds"))
            }
          }

          d_se <- datasets %>%
            dplyr::mutate(TigacellID = .data$TigacellID_small) %>%
            dplyr::filter(.data$year >= 2018)

          joinable_effort <- trs_daily %>%
            sf::st_drop_geometry()

          join_cols <- intersect(c("TigacellID", "date"), names(joinable_effort))
          if (length(join_cols) > 0) {
            d_se <- d_se %>%
              dplyr::left_join(joinable_effort, by = join_cols)
          }
        }
      }
    }
  }

  attr(datasets, "season_bounds") <- season_bounds
  attr(datasets, "season_bounds_yearly") <- season_bounds_yearly
  attr(datasets, "trs_daily") <- trs_daily
  attr(datasets, "min_SE_logit") <- min_SE_logit
  attr(datasets, "sampling_effort_joined") <- d_se

  attr(datasets, "sampling_effort") <- sampling_effort
  attr(datasets, "hex_grid") <- hex_grid
  attr(datasets, "output_path") <- output_path

  message("Finished preparing dataset: ", output_path)
  datasets
}