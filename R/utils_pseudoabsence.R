# Add the requested model grid to a TRS or TGB effort dataset.
prepare_pseudoabsence_effort <- function(
    effort,
    effort_path,
    cell_id_col,
    iso3,
    admin_level,
    admin_name,
    data_dir,
    location_slug,
    lon_col = "lon",
    lat_col = "lat"
) {
  if (cell_id_col %in% names(effort)) return(effort)

  if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
    stop(
      "`iso3`, `admin_level`, and `admin_name` are required to add `",
      cell_id_col, "` to the effort dataset.",
      call. = FALSE
    )
  }
  missing_coordinates <- !all(c(lon_col, lat_col) %in% names(effort))

  if (missing_coordinates && inherits(effort, "sf")) {
    effort_wgs84 <- sf::st_transform(effort, 4326)
    coordinates <- sf::st_coordinates(effort_wgs84)

    if (nrow(coordinates) != nrow(effort)) {
      stop("Effort geometry must contain one point per row.", call. = FALSE)
    }

    effort[[lon_col]] <- coordinates[, 1]
    effort[[lat_col]] <- coordinates[, 2]
  }

  if (!all(c(lon_col, lat_col) %in% names(effort))) {
    stop("Effort dataset must contain `", lon_col, "` and `", lat_col, "`.", call. = FALSE)
  }

  attr(effort, "output_path") <- effort_path
  attr(effort, "location_slug") <- location_slug

  if (grepl("^h3_id_[0-9]+$", cell_id_col)) {
    effort <- add_h3_grid(
      dataset = effort,
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      grid_dir = data_dir,
      resolution = as.integer(sub("^h3_id_", "", cell_id_col)),
      verbose = FALSE,
      write_output = FALSE
    )
  } else if (grepl("^hex_id_[0-9]+(_[0-9]+)?$", cell_id_col)) {
    token <- sub("^hex_id_", "", cell_id_col)
    effort <- add_hex_grid(
      dataset = effort,
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      grid_dir = data_dir,
      cellsize_m = as.numeric(sub("_", ".", token, fixed = TRUE)),
      verbose = FALSE,
      write_output = FALSE
    )

    old_names <- c(paste0("grid_id_", token), paste0("grid_id*", token))
    old_name <- old_names[old_names %in% names(effort)][1]
    if (!cell_id_col %in% names(effort) && !is.na(old_name)) {
      names(effort)[names(effort) == old_name] <- cell_id_col
    }
  } else {
    stop(
      "Cannot infer a grid from `", cell_id_col,
      "`. Use a name such as `h3_id_9` or `hex_id_1200`.",
      call. = FALSE
    )
  }

  if (!cell_id_col %in% names(effort)) {
    stop("Grid enrichment did not create `", cell_id_col, "`.", call. = FALSE)
  }

  if (!inherits(effort, "sf")) {
    effort <- sf::st_as_sf(
      effort,
      coords = c(lon_col, lat_col),
      crs = 4326,
      remove = FALSE
    )
  }

  effort
}


# Remove background candidates that overlap a known presence.
exclude_known_presence_candidates <- function(
    eff_sf,
    known_presences,
    source_value,
    temporal_resolution,
    cell_id_col,
    date_col = "date",
    hour_col = "hour"
) {
  if (!nrow(eff_sf) || !nrow(known_presences)) return(eff_sf)

  pres_key <- paste(
    known_presences[[cell_id_col]],
    known_presences[[date_col]],
    sep = "|"
  )
  cand_key <- paste(
    eff_sf[[cell_id_col]],
    eff_sf[[date_col]],
    sep = "|"
  )

  use_candidate_hour <- temporal_resolution == "hourly" &&
    hour_col %in% names(eff_sf) &&
    !anyNA(eff_sf[[hour_col]])

  if (use_candidate_hour) {
    pres_key <- paste(pres_key, known_presences[[hour_col]], sep = "|")
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


# Fill missing pseudoabsence hours using observed hours from the same source.
add_observed_hours <- function(
    sampled,
    known_presences,
    source_value,
    temporal_resolution,
    source_col = "source",
    date_col = "date",
    hour_col = "hour"
) {
  if (temporal_resolution != "hourly") return(sampled)

  if (!hour_col %in% names(sampled)) sampled[[hour_col]] <- NA_integer_
  missing_hour <- is.na(sampled[[hour_col]])

  if (any(missing_hour)) {
    observed_hours <- known_presences |>
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

    sampled[[hour_col]][missing_hour] <- observed_hours[
      sample.int(
        length(observed_hours),
        sum(missing_hour),
        replace = TRUE
      )
    ]
  }

  sampled$datetime <- as.POSIXct(
    paste(sampled[[date_col]], sprintf("%02d:00:00", sampled[[hour_col]])),
    tz = "UTC"
  )

  sampled
}


# Draw weighted pseudoabsences from one effort dataset.
sample_from_effort <- function(
    eff_sf,
    known_presences,
    weight_col,
    n_abs,
    source_value,
    pa_method,
    temporal_resolution,
    cell_id_col,
    date_col = "date",
    hour_col = "hour",
    lon_col = "lon",
    lat_col = "lat",
    source_col = "source",
    extra_keep = character()
) {
  n_abs <- as.integer(round(n_abs))
  if (n_abs <= 0) return(NULL)
  if (!inherits(eff_sf, "sf")) {
    stop("Effort surface must be an sf object.", call. = FALSE)
  }

  required <- c(date_col, weight_col, cell_id_col)
  missing <- setdiff(required, names(eff_sf))
  if (length(missing)) {
    stop(
      "Effort surface for ", source_value, " missing required column(s): ",
      paste(missing, collapse = ", "),
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

  years_use <- unique(lubridate::year(known_presences[[date_col]]))
  eff_sf$year <- lubridate::year(eff_sf[[date_col]])
  eff_sf <- eff_sf |>
    dplyr::filter(
      .data$year %in% years_use,
      .data[[weight_col]] > 0,
      !is.na(.data[[cell_id_col]]),
      nzchar(.data[[cell_id_col]])
    )

  if (!nrow(eff_sf)) {
    stop("No positive-effort rows for source: ", source_value, call. = FALSE)
  }

  eff_sf <- exclude_known_presence_candidates(
    eff_sf = eff_sf,
    known_presences = known_presences,
    source_value = source_value,
    temporal_resolution = temporal_resolution,
    cell_id_col = cell_id_col,
    date_col = date_col,
    hour_col = hour_col
  )

  if (!nrow(eff_sf)) {
    stop(
      "No background candidates remain after excluding known presences for source ",
      source_value, ".",
      call. = FALSE
    )
  }

  sampled <- eff_sf |>
    dplyr::slice_sample(
      n = n_abs,
      replace = TRUE,
      weight_by = .data[[weight_col]]
    )

  keep_cols <- unique(c(
    "year", date_col, weight_col, cell_id_col,
    hour_col, extra_keep
  ))
  keep_cols <- intersect(keep_cols, names(sampled))

  sampled <- sampled |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::mutate(
      presence = FALSE,
      !!source_col := source_value,
      pa_method = pa_method
    )

  sampled <- add_observed_hours(
    sampled = sampled,
    known_presences = known_presences,
    source_value = source_value,
    temporal_resolution = temporal_resolution,
    source_col = source_col,
    date_col = date_col,
    hour_col = hour_col
  )

  xy <- sf::st_coordinates(sampled)
  sampled[[lon_col]] <- xy[, 1]
  sampled[[lat_col]] <- xy[, 2]

  sampled
}
