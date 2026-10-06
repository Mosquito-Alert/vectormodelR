#' Generate pseudoabsences using TRS effort and TGB weights
#'
#' Samples pseudoabsences separately for Mosquito Alert and GBIF. Before
#' sampling, the requested model grid is added to each effort dataset and
#' candidates overlapping known presences are removed.
#'
#' @param dataset In-memory modelling dataset or path to an RDS file.
#' @param iso3 Three-letter country code.
#' @param admin_level Administrative level.
#' @param admin_name Administrative-area name.
#' @param data_dir Directory containing processed datasets.
#' @param temporal_resolution Either `"daily"` or `"hourly"`.
#' @param sampling_factor_ma Pseudoabsences per Mosquito Alert presence.
#' @param sampling_factor_gbif Pseudoabsences per GBIF presence.
#' @param cell_id_col Spatial-cell column, such as `h3_id_9` or `hex_id_1200`.
#' @param date_col Date column.
#' @param hour_col Hour column.
#' @param lon_col Longitude column.
#' @param lat_col Latitude column.
#' @param source_col Data-source column.
#' @param se_col TRS sampling-effort weight column.
#' @param tgb_col GBIF target-group background weight column.
#' @param ma_source Mosquito Alert source label.
#' @param gbif_source GBIF source label.
#' @param write_output Whether to save the result.
#'
#' @return A tibble containing known presences and generated pseudoabsences.
#' @export
add_pseudoabsences_se <- function(
    dataset,
    iso3                 = NULL,
    admin_level          = NULL,
    admin_name           = NULL,
    data_dir             = "data/proc",
    temporal_resolution  = c("daily", "hourly"),
    sampling_factor_ma   = 10,
    sampling_factor_gbif = 10,
    cell_id_col          = "cell_id",
    date_col             = "date",
    hour_col             = "hour",
    lon_col              = "lon",
    lat_col              = "lat",
    source_col           = "source",
    se_col               = "SE_expected",
    tgb_col              = "tgb_w",
    ma_source            = "malert",
    gbif_source          = "gbif",
    write_output         = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)

  for (pkg in c("dplyr", "sf", "lubridate")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required.", call. = FALSE)
    }
  }

  infer_slug <- function(path) {
    stem <- tools::file_path_sans_ext(basename(path))
    match <- regexec("^model_prep_(.+?)_(malert|gbif)(_.*)?$", stem)
    parts <- regmatches(stem, match)[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }

  # ---- 1) Load and validate known presences ----
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L

  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) {
      stop("Model dataset not found at ", dataset_path, call. = FALSE)
    }
    D <- readRDS(dataset_path)
  } else {
    D <- dataset
    dataset_path <- attr(D, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop(
        "Input dataset must carry an `output_path` attribute or be an RDS path.",
        call. = FALSE
      )
    }
  }

  location_slug <- attr(D, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) {
    location_slug <- infer_slug(dataset_path)
  }
  if (is.na(location_slug) || !nzchar(location_slug)) {
    stop("Could not determine the location slug.", call. = FALSE)
  }

  required <- c(lon_col, lat_col, date_col, cell_id_col, "presence")
  if (temporal_resolution == "hourly") required <- c(required, hour_col)
  missing <- setdiff(required, names(D))
  if (length(missing)) {
    stop(
      "Dataset missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!source_col %in% names(D)) {
    warning("Dataset missing `", source_col, "`; treating all records as Mosquito Alert.")
    D[[source_col]] <- ma_source
  }

  D[[lon_col]] <- suppressWarnings(as.numeric(D[[lon_col]]))
  D[[lat_col]] <- suppressWarnings(as.numeric(D[[lat_col]]))
  D[[date_col]] <- as.Date(D[[date_col]])
  D[[cell_id_col]] <- trimws(as.character(D[[cell_id_col]]))

  if (temporal_resolution == "hourly") {
    D[[hour_col]] <- suppressWarnings(as.integer(D[[hour_col]]))
  }

  valid <- !is.na(D[[lon_col]]) &
    !is.na(D[[lat_col]]) &
    !is.na(D[[date_col]]) &
    !is.na(D[[cell_id_col]]) &
    nzchar(D[[cell_id_col]])

  if (temporal_resolution == "hourly") {
    valid_hour <- !is.na(D[[hour_col]]) &
      D[[hour_col]] >= 0L &
      D[[hour_col]] <= 23L
    valid <- valid & valid_hour
  }

  if (any(!valid)) {
    message("Excluded ", sum(!valid), " records with invalid spatial or temporal values.")
    D <- D[valid, , drop = FALSE]
  }

  presence_text <- tolower(trimws(as.character(D$presence)))
  known_presence <- presence_text %in% c("true", "1", "present")

  if (any(!known_presence)) {
    message("Excluded ", sum(!known_presence), " non-presence input records.")
    D <- D[known_presence, , drop = FALSE]
  }
  if (!nrow(D)) {
    stop("No known presences remain after filtering.", call. = FALSE)
  }

  D$presence <- TRUE
  D_sf <- sf::st_as_sf(
    D,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )
  D_sf$year <- lubridate::year(D_sf[[date_col]])

  D_ma <- D_sf |> dplyr::filter(.data[[source_col]] == ma_source)
  D_gbif <- D_sf |> dplyr::filter(.data[[source_col]] == gbif_source)
  n_pres_ma <- nrow(D_ma)
  n_pres_gbif <- nrow(D_gbif)
  suffix <- temporal_resolution

  # ---- 2) Mosquito Alert pseudoabsences ----
  abs_ma <- NULL

  if (n_pres_ma > 0 && sampling_factor_ma > 0) {
    trs_path <- file.path(
      data_dir,
      sprintf("model_prep_%s_trs_%s.Rds", location_slug, suffix)
    )
    if (!file.exists(trs_path) && temporal_resolution == "hourly") {
      trs_path <- file.path(
        data_dir,
        sprintf("model_prep_%s_trs_daily.Rds", location_slug)
      )
    }
    if (!file.exists(trs_path)) {
      stop("TRS dataset not found at ", trs_path, call. = FALSE)
    }

    trs_effort <- prepare_pseudoabsence_effort(
      effort = readRDS(trs_path),
      effort_path = trs_path,
      cell_id_col = cell_id_col,
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      data_dir = data_dir,
      location_slug = location_slug,
      lon_col = lon_col,
      lat_col = lat_col
    )

    se_cols <- unique(c(
      se_col,
      intersect(c("SE", "SE_expected"), names(trs_effort))
    ))

    abs_ma <- sample_from_effort(
      eff_sf = trs_effort,
      known_presences = D_sf,
      weight_col = se_col,
      n_abs = sampling_factor_ma * n_pres_ma,
      source_value = ma_source,
      pa_method = "trs_effort",
      temporal_resolution = temporal_resolution,
      cell_id_col = cell_id_col,
      date_col = date_col,
      hour_col = hour_col,
      lon_col = lon_col,
      lat_col = lat_col,
      source_col = source_col,
      extra_keep = se_cols
    )

    if (!is.null(abs_ma)) {
      for (column in se_cols) {
        if (!column %in% names(D_sf)) D_sf[[column]] <- NA_real_
      }
    }
  }

  # ---- 3) GBIF pseudoabsences ----
  abs_gbif <- NULL

  if (n_pres_gbif > 0 && sampling_factor_gbif > 0) {
    tgb_path <- file.path(
      data_dir,
      sprintf("model_prep_%s_tgb_%s.Rds", location_slug, suffix)
    )
    if (!file.exists(tgb_path) && temporal_resolution == "hourly") {
      tgb_path <- file.path(
        data_dir,
        sprintf("model_prep_%s_tgb_daily.Rds", location_slug)
      )
    }
    if (!file.exists(tgb_path)) {
      stop("TGB dataset not found at ", tgb_path, call. = FALSE)
    }

    tgb_effort <- prepare_pseudoabsence_effort(
      effort = readRDS(tgb_path),
      effort_path = tgb_path,
      cell_id_col = cell_id_col,
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      data_dir = data_dir,
      location_slug = location_slug,
      lon_col = lon_col,
      lat_col = lat_col
    )

    abs_gbif <- sample_from_effort(
      eff_sf = tgb_effort,
      known_presences = D_sf,
      weight_col = tgb_col,
      n_abs = sampling_factor_gbif * n_pres_gbif,
      source_value = gbif_source,
      pa_method = "gbif_tgb",
      temporal_resolution = temporal_resolution,
      cell_id_col = cell_id_col,
      date_col = date_col,
      hour_col = hour_col,
      lon_col = lon_col,
      lat_col = lat_col,
      source_col = source_col,
      extra_keep = tgb_col
    )
  }

  # ---- 4) Combine and save ----
  pres_df <- sf::st_drop_geometry(D_sf) |>
    dplyr::mutate(pa_method = NA_character_)

  abs_df <- dplyr::bind_rows(
    if (!is.null(abs_ma)) sf::st_drop_geometry(abs_ma) else NULL,
    if (!is.null(abs_gbif)) sf::st_drop_geometry(abs_gbif) else NULL
  )
  combined <- dplyr::bind_rows(pres_df, abs_df)

  base_attrs <- attributes(D)
  stem <- tools::file_path_sans_ext(basename(dataset_path))
  output_path <- file.path(
    data_dir,
    paste0(stem, "_se_", temporal_resolution, ".Rds")
  )

  preserve <- base_attrs[
    setdiff(names(base_attrs), c("names", "row.names", "class"))
  ]
  for (name in names(preserve)) attr(combined, name) <- preserve[[name]]

  attr(combined, "output_path") <- output_path
  attr(combined, "location_slug") <- location_slug
  attr(combined, "temporal_resolution") <- temporal_resolution

  if (isTRUE(write_output)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(combined, output_path)
  }

  combined
}
