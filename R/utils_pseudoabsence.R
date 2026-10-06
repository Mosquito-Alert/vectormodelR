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


#' Expand coarse TRS effort cells onto a model grid
#'
#' Converts daily TRS sampling-effort data recorded on coarse geographic cells
#' into an effort surface using the cells of a selected H3 or regular hexagonal
#' model grid.
#'
#' Each unique TRS location is treated as the centre of a square cell whose
#' width and height are given by `sampling_cell_degrees`. Model-grid cells are
#' assigned the TRS effort values of the coarse cell containing their centre.
#'
#' @param trs_daily An `sf` object containing the daily TRS effort data. It must
#'   contain `masked_lon`, `masked_lat`, and `date`. Any additional columns,
#'   including sampling-effort values such as `SE_expected`, are retained.
#' @param grid Character scalar identifying the target model grid. H3 grids use
#'   the form `"h3_<resolution>"`, for example `"h3_9"`. Regular hexagonal grids
#'   use the form `"hex_<cellsize>"`, for example `"hex_1200"`.
#' @param iso3 Three-letter ISO country code used to identify the study area.
#' @param admin_level Administrative level of the study area.
#' @param admin_name Name of the administrative area.
#' @param data_dir Directory containing the saved spatial grids. Defaults to
#'   `"data/proc"`.
#' @param sampling_cell_degrees Width and height, in decimal degrees, of the
#'   original square TRS sampling cells. Defaults to `0.025`.
#'
#' @return An `sf` object with point geometry containing one row for every
#'   matched model-grid cell and TRS daily effort record. The result includes
#'   `lon`, `lat`, the appropriate model-grid identifier, and the columns from
#'   `trs_daily`.
#'
#'   The following attributes are added:
#'
#'   * `"grid"`: the supplied grid specification.
#'   * `"cell_id_col"`: the name of the generated model-cell identifier column.
#'
#' @details
#' The selected model grid is loaded from `data_dir`. If an H3 grid is not
#' already available, it is created with `build_h3_grid()`. Hexagonal grids
#' must already exist.
#'
#' Spatial matching uses the centre point of each model-grid cell. Consequently,
#' the returned geometry remains point geometry for use by downstream
#' pseudoabsence sampling functions.
#'
#' Because each coarse TRS cell can contain many model-grid cells, the returned
#' dataset can be substantially larger than `trs_daily`. This is the intended
#' expansion of the effort surface.
#'
#' @noRd
expand_trs_to_model_grid <- function(
    trs_daily,
    grid,
    iso3,
    admin_level,
    admin_name,
    data_dir = "data/proc",
    sampling_cell_degrees = 0.025
) {
  grid <- tolower(trimws(grid))
  is_h3 <- grepl("^h3_[0-9]+$", grid)
  is_hex <- grepl("^hex_[0-9]+(\\.[0-9]+)?$", grid)

  if (!is_h3 && !is_hex) {
    stop("`grid` must look like `h3_9` or `hex_1200`.", call. = FALSE)
  }
  if (!inherits(trs_daily, "sf")) {
    stop("`trs_daily` must be an sf object.", call. = FALSE)
  }
  if (!all(c("masked_lon", "masked_lat", "date") %in% names(trs_daily))) {
    stop(
      "TRS data must contain `masked_lon`, `masked_lat`, and `date`.",
      call. = FALSE
    )
  }

  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  slug <- ids$slug

  # Make one polygon for each original 0.025-degree effort cell.
  effort_centres <- trs_daily |>
    sf::st_drop_geometry() |>
    dplyr::distinct(.data$masked_lon, .data$masked_lat)

  half_cell <- sampling_cell_degrees / 2
  effort_geometry <- mapply(
    function(x, y) {
      sf::st_polygon(list(matrix(
        c(
          x - half_cell, y - half_cell,
          x + half_cell, y - half_cell,
          x + half_cell, y + half_cell,
          x - half_cell, y + half_cell,
          x - half_cell, y - half_cell
        ),
        ncol = 2,
        byrow = TRUE
      )))
    },
    effort_centres$masked_lon,
    effort_centres$masked_lat,
    SIMPLIFY = FALSE
  )

  effort_cells <- sf::st_sf(
    effort_centres,
    geometry = sf::st_sfc(effort_geometry, crs = 4326)
  )

  # Load or create the selected model grid.
  if (is_h3) {
    resolution <- as.integer(sub("^h3_", "", grid))
    if (!resolution %in% 0:15) {
      stop("H3 resolution must be between 0 and 15.", call. = FALSE)
    }

    grid_paths <- file.path(
      data_dir,
      c(
        sprintf("spatial_%s_h3_grid_%s.Rds", slug, resolution),
        sprintf("spatial_%s_h3_grid_%s.rds", slug, resolution)
      )
    )
    grid_path <- grid_paths[file.exists(grid_paths)][1]

    if (is.na(grid_path)) {
      model_grid <- build_h3_grid(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        resolution = resolution,
        data_dir = data_dir,
        write = TRUE,
        verbose = FALSE
      )
    } else {
      model_grid <- readRDS(grid_path)
    }

    cell_id_col <- paste0("h3_id_", resolution)
    if (!cell_id_col %in% names(model_grid)) {
      if (!"h3_id" %in% names(model_grid)) {
        stop("H3 grid does not contain an H3 identifier.", call. = FALSE)
      }
      model_grid[[cell_id_col]] <- model_grid$h3_id
    }
  } else {
    cellsize_m <- as.numeric(sub("^hex_", "", grid))
    token <- gsub(
      "\\.",
      "_",
      format(cellsize_m, trim = TRUE, scientific = FALSE)
    )

    grid_paths <- file.path(
      data_dir,
      c(
        sprintf("spatial_%s_hex_grid_%s.Rds", slug, token),
        sprintf("spatial_%s_hex_grid_%s.rds", slug, token),
        sprintf("spatial_%s_hex_grid.Rds", slug),
        sprintf("spatial_%s_hex_grid.rds", slug)
      )
    )
    grid_path <- grid_paths[file.exists(grid_paths)][1]

    if (is.na(grid_path)) {
      stop("Hex grid not found for `", grid, "`.", call. = FALSE)
    }

    model_grid <- readRDS(grid_path)
    cell_id_col <- paste0("hex_id_", token)
    source_id_col <- c(
      cell_id_col,
      paste0("grid_id_", token),
      "grid_id"
    )
    source_id_col <- source_id_col[source_id_col %in% names(model_grid)][1]

    if (is.na(source_id_col)) {
      stop("Hex grid does not contain a grid identifier.", call. = FALSE)
    }
    model_grid[[cell_id_col]] <- model_grid[[source_id_col]]
  }

  model_grid <- sf::st_as_sf(model_grid) |>
    sf::st_transform(4326)

  # Use one point per model cell so downstream sampling retains point geometry.
  model_centres <- suppressWarnings(
    sf::st_point_on_surface(model_grid[cell_id_col])
  )
  model_centres <- sf::st_join(
    model_centres,
    effort_cells,
    join = sf::st_within,
    left = FALSE
  )

  if (!nrow(model_centres)) {
    stop("No model cells matched the TRS effort cells.", call. = FALSE)
  }

  coordinates <- sf::st_coordinates(model_centres)
  model_centres$lon <- coordinates[, 1]
  model_centres$lat <- coordinates[, 2]

  effort_values <- sf::st_drop_geometry(trs_daily)
  expanded <- model_centres |>
    dplyr::left_join(
      effort_values,
      by = c("masked_lon", "masked_lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(!is.na(.data$date))

  attr(expanded, "grid") <- grid
  attr(expanded, "cell_id_col") <- cell_id_col
  expanded
}


#' Parse a sampling-effort feature code
#'
#' Parses `"se"`, `"se_<factor>"`, and
#' `"se_<trs_factor>_<tgb_factor>"` feature codes.
#'
#' @param feature Character feature code.
#'
#' @return `NULL` if the feature is not an `se` feature. Otherwise, a parsed
#'   feature-specification list.
#'
#' @noRd
parse_se_feature <- function(feature) {
  feature <- tolower(trimws(feature))

  if (!grepl("^se(?:_|$)", feature)) {
    return(NULL)
  }

  if (!grepl("^se(?:_[0-9]+(?:\\.[0-9]+)?){0,2}$", feature)) {
    stop(
      "Invalid sampling-effort feature `", feature,
      "`. Use `se`, `se_7`, or `se_7_5`.",
      call. = FALSE
    )
  }

  parts <- strsplit(feature, "_", fixed = TRUE)[[1]]
  factors <- suppressWarnings(as.numeric(parts[-1]))

  if (length(factors) &&
      any(!is.finite(factors) | factors <= 0)) {
    stop(
      "Sampling factors in `", feature,
      "` must be positive numbers.",
      call. = FALSE
    )
  }

  sampling_factor_ma <- NULL
  sampling_factor_gbif <- NULL

  if (length(factors) == 1L) {
    sampling_factor_ma <- factors[1]
    sampling_factor_gbif <- factors[1]
  }

  if (length(factors) == 2L) {
    sampling_factor_ma <- factors[1]
    sampling_factor_gbif <- factors[2]
  }

  list(
    code = "se",
    value = NA_real_,
    raw = feature,
    cell_id_col = NA_character_,
    sampling_factor_ma = sampling_factor_ma,
    sampling_factor_gbif = sampling_factor_gbif
  )
}