#' Generate pseudoabsences using TRS effort (Mosquito Alert) and TGB (GBIF)
#'
#' Samples pseudoabsences separately for Mosquito Alert (TRS-based effort
#' surface) and GBIF (target-group background weights). Candidate backgrounds
#' matching any known presence in the same spatial and temporal unit are
#' excluded before sampling.
#'
#' @param dataset Either the in-memory modelling dataset or an RDS path.
#' @param data_dir Directory holding processed datasets and outputs.
#' @param temporal_resolution Either `"daily"` or `"hourly"`.
#' @param sampling_factor_ma Mosquito Alert pseudoabsences per MA presence.
#' @param sampling_factor_gbif GBIF pseudoabsences per GBIF presence.
#' @param cell_id_col Name of the spatial-cell column shared by the presence and
#'   effort datasets. The column is required in both datasets.
#' @param date_col Name of the date column.
#' @param hour_col Name of the hour column.
#' @param lon_col Name of the longitude column.
#' @param lat_col Name of the latitude column.
#' @param source_col Name of the source column.
#' @param se_col Name of the TRS effort column.
#' @param tgb_col Name of the GBIF target-group background weight column.
#' @param ma_source Label used for Mosquito Alert records.
#' @param gbif_source Label used for GBIF records.
#' @param write_output Whether to save the result.
#'
#' @return A tibble combining presences and generated pseudoabsences.
#' @export
add_pseudoabsences_se <- function(
    dataset,
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
      stop("Package '", pkg, "' is required. Install with install.packages('", pkg, "').", call. = FALSE)
    }
  }

  infer_slug <- function(path) {
    fname <- basename(path)
    matches <- regexec("^model_prep_(.+?)_(?:malert|gbif)(?:_.*)?$", fname)
    parts <- regmatches(fname, matches)[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }

  # ---- 1) Load and validate the known-presence dataset ----
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) stop("Model dataset not found at ", dataset_path, call. = FALSE)
    D <- readRDS(dataset_path)
  } else {
    D <- dataset
    dataset_path <- attr(D, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop("Input dataset must carry an `output_path` attribute or be a file path.", call. = FALSE)
    }
  }

  location_slug <- attr(D, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) location_slug <- infer_slug(dataset_path)
  if (is.na(location_slug) || !nzchar(location_slug)) {
    stop("Could not determine location slug; ensure `location_slug` attribute exists.", call. = FALSE)
  }
  attr(D, "location_slug") <- location_slug

  req <- c(lon_col, lat_col, date_col, cell_id_col, "presence")
  if (temporal_resolution == "hourly") req <- c(req, hour_col)
  miss <- setdiff(req, names(D))
  if (length(miss)) stop("Dataset missing required column(s): ", paste(miss, collapse = ", "), call. = FALSE)

  if (!source_col %in% names(D)) {
    warning("Dataset missing `", source_col, "`; treating all presences as Mosquito Alert.")
    D[[source_col]] <- ma_source
  }

  D[[lon_col]] <- suppressWarnings(as.numeric(D[[lon_col]]))
  D[[lat_col]] <- suppressWarnings(as.numeric(D[[lat_col]]))
  D[[date_col]] <- as.Date(D[[date_col]])
  D[[cell_id_col]] <- trimws(as.character(D[[cell_id_col]]))
  if (temporal_resolution == "hourly") D[[hour_col]] <- suppressWarnings(as.integer(D[[hour_col]]))

  complete_input <- !is.na(D[[lon_col]]) & !is.na(D[[lat_col]]) &
    !is.na(D[[date_col]]) & !is.na(D[[cell_id_col]]) & nzchar(D[[cell_id_col]])
  if (temporal_resolution == "hourly") {
    valid_hour <- !is.na(D[[hour_col]]) & D[[hour_col]] >= 0L & D[[hour_col]] <= 23L
    complete_input <- complete_input & valid_hour
  }
  if (any(!complete_input)) {
    message("Excluded ", sum(!complete_input), " input records with missing or invalid spatial/temporal keys.")
    D <- D[complete_input, , drop = FALSE]
  }

  presence_text <- tolower(trimws(as.character(D$presence)))
  known_presence <- presence_text %in% c("true", "1", "present")
  if (any(!known_presence)) {
    message("Excluded ", sum(!known_presence), " non-presence input records.")
    D <- D[known_presence, , drop = FALSE]
  }
  if (!nrow(D)) stop("No known presence records remain after filtering.", call. = FALSE)
  D$presence <- TRUE

  D_sf <- sf::st_as_sf(D, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  D_sf$year <- lubridate::year(D_sf[[date_col]])
  years_use <- sort(unique(stats::na.omit(D_sf$year)))

  # ---- helper: exclude candidates overlapping any known presence ----
  exclude_known_presence_candidates <- function(eff_sf, source_value) {
    if (!nrow(eff_sf) || !nrow(D_sf)) return(eff_sf)

    pres_key <- paste(D_sf[[cell_id_col]], D_sf[[date_col]], sep = "|")
    cand_key <- paste(eff_sf[[cell_id_col]], eff_sf[[date_col]], sep = "|")

    # Use hour only when all effort candidates have valid hours. Otherwise,
    # exclude conservatively at the cell-date level before assigning hours.
    use_candidate_hour <- temporal_resolution == "hourly" &&
      hour_col %in% names(eff_sf) && !anyNA(eff_sf[[hour_col]])

    if (use_candidate_hour) {
      pres_key <- paste(pres_key, D_sf[[hour_col]], sep = "|")
      cand_key <- paste(cand_key, eff_sf[[hour_col]], sep = "|")
    }

    overlap <- cand_key %in% pres_key
    if (any(overlap)) {
      message(
        "Excluded ", sum(overlap), " ", source_value,
        " background candidates overlapping known presence cells/times."
      )
      eff_sf <- eff_sf[!overlap, , drop = FALSE]
    }
    eff_sf
  }

  # ---- helper: sample missing background hours from observed hours ----
  add_observed_hours <- function(sampled, source_value) {
    if (temporal_resolution != "hourly") return(sampled)

    if (!hour_col %in% names(sampled)) sampled[[hour_col]] <- NA_integer_
    missing_hour <- is.na(sampled[[hour_col]])
    if (any(missing_hour)) {
      observed_hours <- D_sf |>
        dplyr::filter(
          .data[[source_col]] == source_value,
          !is.na(.data[[hour_col]])
        ) |>
        dplyr::pull(.data[[hour_col]])

      if (!length(observed_hours)) {
        stop(
          "Cannot generate hourly backgrounds for ", source_value,
          " because that source has no observed hours.",
          call. = FALSE
        )
      }
      sampled[[hour_col]][missing_hour] <- sample(
        observed_hours,
        sum(missing_hour),
        replace = TRUE
      )
    }

    sampled$datetime <- as.POSIXct(
      paste(sampled[[date_col]], sprintf("%02d:00:00", sampled[[hour_col]])),
      tz = "UTC"
    )
    sampled
  }

  # ---- helper: weighted pseudoabsence sampling ----
  sample_from_effort <- function(eff_sf, weight_col, n_abs, source_value, pa_method, extra_keep = character()) {
    n_abs <- as.integer(round(n_abs))
    if (n_abs <= 0) return(NULL)
    if (!inherits(eff_sf, "sf")) stop("Effort surface must be an sf object.", call. = FALSE)

    req_eff <- c(date_col, weight_col, cell_id_col)
    missing_eff <- setdiff(req_eff, names(eff_sf))
    if (length(missing_eff)) {
      stop(
        "Effort surface for ", source_value, " missing required column(s): ",
        paste(missing_eff, collapse = ", "),
        call. = FALSE
      )
    }

    eff_sf[[date_col]] <- as.Date(eff_sf[[date_col]])
    eff_sf[[cell_id_col]] <- trimws(as.character(eff_sf[[cell_id_col]]))
    if (hour_col %in% names(eff_sf)) {
      eff_sf[[hour_col]] <- suppressWarnings(as.integer(eff_sf[[hour_col]]))
      invalid_hour <- !is.na(eff_sf[[hour_col]]) &
        (eff_sf[[hour_col]] < 0L | eff_sf[[hour_col]] > 23L)
      eff_sf[[hour_col]][invalid_hour] <- NA_integer_
    }
    eff_sf$year <- lubridate::year(eff_sf[[date_col]])
    eff_sf <- eff_sf |>
      dplyr::filter(
        .data$year %in% years_use,
        .data[[weight_col]] > 0,
        !is.na(.data[[cell_id_col]]),
        nzchar(.data[[cell_id_col]])
      )

    if (!nrow(eff_sf)) stop("No positive-effort rows for source: ", source_value, call. = FALSE)

    # All known presences are used, regardless of their source.
    eff_sf <- exclude_known_presence_candidates(eff_sf, source_value)
    if (!nrow(eff_sf)) {
      stop(
        "No background candidates remain after excluding known presences for source ",
        source_value,
        ". The effort surface may contain only focal-species presences.",
        call. = FALSE
      )
    }

    sampled <- eff_sf |>
      dplyr::slice_sample(n = n_abs, replace = TRUE, weight_by = .data[[weight_col]])

    keep_cols <- unique(c("year", date_col, weight_col, cell_id_col, hour_col, extra_keep))
    keep_cols <- intersect(keep_cols, names(sampled))
    sampled <- sampled |>
      dplyr::select(dplyr::all_of(keep_cols)) |>
      dplyr::mutate(
        presence = FALSE,
        !!source_col := source_value,
        pa_method = pa_method
      )

    sampled <- add_observed_hours(sampled, source_value)
    xy <- sf::st_coordinates(sampled)
    sampled[[lon_col]] <- xy[, 1]
    sampled[[lat_col]] <- xy[, 2]
    sampled
  }

  # ---- 2) Split known presences ----
  D_ma <- D_sf |> dplyr::filter(.data[[source_col]] == ma_source)
  D_gbif <- D_sf |> dplyr::filter(.data[[source_col]] == gbif_source)
  n_pres_ma <- nrow(D_ma)
  n_pres_gbif <- nrow(D_gbif)

  # ---- 3) Mosquito Alert pseudoabsences ----
  abs_ma <- NULL
  if (n_pres_ma > 0 && sampling_factor_ma > 0) {
    suffix <- if (temporal_resolution == "hourly") "hourly" else "daily"
    trs_path <- file.path(data_dir, sprintf("model_prep_%s_trs_%s.Rds", location_slug, suffix))
    if (!file.exists(trs_path) && temporal_resolution == "hourly") {
      trs_path <- file.path(data_dir, sprintf("model_prep_%s_trs_daily.Rds", location_slug))
    }
    if (!file.exists(trs_path)) stop("TRS dataset not found at ", trs_path, call. = FALSE)

    trs_effort <- readRDS(trs_path)
    se_cols <- unique(c(se_col, intersect(c("SE", "SE_expected"), names(trs_effort))))
    abs_ma <- sample_from_effort(
      eff_sf = trs_effort,
      weight_col = se_col,
      n_abs = sampling_factor_ma * n_pres_ma,
      source_value = ma_source,
      pa_method = "trs_effort",
      extra_keep = se_cols
    )

    if (!is.null(abs_ma)) {
      for (col in se_cols) if (!col %in% names(D_sf)) D_sf[[col]] <- NA_real_
    }
  }

  # ---- 4) GBIF pseudoabsences ----
  abs_gbif <- NULL
  if (n_pres_gbif > 0 && sampling_factor_gbif > 0) {
    suffix <- if (temporal_resolution == "hourly") "hourly" else "daily"
    tgb_path <- file.path(data_dir, sprintf("model_prep_%s_tgb_%s.Rds", location_slug, suffix))
    if (!file.exists(tgb_path) && temporal_resolution == "hourly") {
      tgb_path <- file.path(data_dir, sprintf("model_prep_%s_tgb_daily.Rds", location_slug))
    }
    if (!file.exists(tgb_path)) stop("TGB dataset not found at ", tgb_path, call. = FALSE)

    tgb_effort <- readRDS(tgb_path)
    abs_gbif <- sample_from_effort(
      eff_sf = tgb_effort,
      weight_col = tgb_col,
      n_abs = sampling_factor_gbif * n_pres_gbif,
      source_value = gbif_source,
      pa_method = "gbif_tgb",
      extra_keep = tgb_col
    )
  }

  # ---- 5) Combine and save ----
  pres_df <- sf::st_drop_geometry(D_sf) |>
    dplyr::mutate(pa_method = NA_character_)

  abs_df <- dplyr::bind_rows(
    if (!is.null(abs_ma)) sf::st_drop_geometry(abs_ma) else NULL,
    if (!is.null(abs_gbif)) sf::st_drop_geometry(abs_gbif) else NULL
  )
  combined <- dplyr::bind_rows(pres_df, abs_df)

  base_attrs <- attributes(D)
  stem <- tools::file_path_sans_ext(basename(dataset_path))
  output_filename <- paste0(stem, "_se_", temporal_resolution, ".Rds")
  output_path <- file.path(data_dir, output_filename)

  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) attr(combined, nm) <- preserve[[nm]]
  attr(combined, "output_path") <- output_path
  attr(combined, "location_slug") <- location_slug
  attr(combined, "temporal_resolution") <- temporal_resolution

  if (isTRUE(write_output)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(combined, output_path)
  }

  combined
}
