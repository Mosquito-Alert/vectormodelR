#' Build GBIF target-group background daily weights on the TRS/Tigacell geometry
#'
#' Creates `model_prep_<iso3>_<admin_level>_<admin_name>_tgb_daily.Rds` by
#' matching GBIF occurrences to the nearest TRS/Tigacell point and aggregating
#' daily counts.
#'
#' @param iso3 Three-letter ISO3 country code used in artefact names.
#' @param admin_level Administrative level identifier used in artefact names.
#' @param admin_name Administrative unit name used in artefact names.
#' @param vector_dir Directory containing `vector_<slug>_gbif.Rds`. Defaults to
#'   "data/proc".
#' @param data_dir Directory containing `model_prep_<slug>_trs_daily.Rds` and
#'   where the TGB output should be written. Defaults to "data/proc".
#' @param weight_col Name of the aggregated weight column. Defaults to "tgb_w".
#' @param time_bin Either "day" (default) or "year"; controls how occurrence
#'   timestamps are binned.
#' @param max_dist_m Optional maximum distance threshold (in meters) for
#'   nearest-neighbour assignments. Occurrences farther than this distance from
#'   their nearest TRS point are dropped.
#' @param write_output Logical; write the output RDS when TRUE (default).
#' @param overwrite Logical; if FALSE (default) and the output already exists,
#'   an error is raised.
#'
#' @return An `sf` object with columns `date`, `year`, and the chosen weight
#'   column. Attributes include `output_path`, `gbif_source`, and `trs_source`.
#' @export
build_tgb_daily <- function(
  iso3,
  admin_level,
  admin_name,
  vector_dir = "data/proc",
  data_dir = "data/proc",
  weight_col = "tgb_w",
  time_bin = c("day", "year"),
  max_dist_m = NULL,
  write_output = TRUE,
  overwrite = FALSE
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install it with install.packages('dplyr').", call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install it with install.packages('sf').", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Install it with install.packages('tibble').", call. = FALSE)
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required. Install it with install.packages('lubridate').", call. = FALSE)
  }

  time_bin <- match.arg(time_bin)
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  slug <- ids$slug

  gbif_path <- file.path(vector_dir, sprintf("vector_%s_gbif.Rds", slug))
  if (!file.exists(gbif_path)) {
    stop("GBIF vector file not found at ", gbif_path, call. = FALSE)
  }
  message("Loaded GBIF occurrences from ", gbif_path)
  gbif_data <- tibble::as_tibble(readRDS(gbif_path))
  if (!nrow(gbif_data)) {
    stop("GBIF file ", gbif_path, " contains no rows after loading.", call. = FALSE)
  }

  normalise_name <- function(x) {
    tolower(gsub("[^a-z0-9]", "", x))
  }
  find_column <- function(data, candidates) {
    col_names <- names(data)
    names_lower <- tolower(col_names)
    names_norm <- normalise_name(col_names)
    for (cand in candidates) {
      cand_lower <- tolower(cand)
      match_idx <- which(names_lower == cand_lower)
      if (length(match_idx)) {
        return(col_names[match_idx[1]])
      }
      norm_idx <- which(names_norm == normalise_name(cand))
      if (length(norm_idx)) {
        return(col_names[norm_idx[1]])
      }
    }
    NA_character_
  }
  parse_event_date <- function(x) {
    if (inherits(x, "Date")) {
      return(as.Date(x))
    }
    x_chr <- trimws(as.character(x))
    x_chr <- sub("T.*$", "", x_chr)
    x_chr[x_chr == ""] <- NA_character_
    suppressWarnings(as.Date(x_chr))
  }

  lon_col <- find_column(gbif_data, c("decimal_longitude", "decimallongitude", "longitude", "lon"))
  lat_col <- find_column(gbif_data, c("decimal_latitude", "decimallatitude", "latitude", "lat"))
  if (is.na(lon_col) || is.na(lat_col)) {
    stop("GBIF data is missing longitude/latitude columns required for spatial assignment.", call. = FALSE)
  }
  event_col <- find_column(gbif_data, c("event_date", "eventdate", "date"))
  year_col <- find_column(gbif_data, "year")

  gbif_filtered <- gbif_data[stats::complete.cases(gbif_data[, c(lon_col, lat_col)]), , drop = FALSE]
  if (!nrow(gbif_filtered)) {
    stop("All GBIF occurrences were missing coordinates; cannot build background.", call. = FALSE)
  }

  event_dates <- if (!is.na(event_col)) parse_event_date(gbif_filtered[[event_col]]) else rep(as.Date(NA), nrow(gbif_filtered))
  year_vals <- if (!is.na(year_col)) suppressWarnings(as.integer(gbif_filtered[[year_col]])) else rep(NA_integer_, nrow(gbif_filtered))

  if (time_bin == "day") {
    if (all(is.na(event_dates)) && all(is.na(year_vals))) {
      stop("No usable eventDate or year information to derive daily bins.", call. = FALSE)
    }
    if (!all(is.na(year_vals))) {
      fallback_dates <- as.Date(paste0(year_vals, "-01-01"))
      repl_idx <- is.na(event_dates) & !is.na(year_vals)
      event_dates[repl_idx] <- fallback_dates[repl_idx]
    }
    gbif_filtered$date <- event_dates
    gbif_filtered$year <- year_vals
    missing_year <- is.na(gbif_filtered$year) & !is.na(gbif_filtered$date)
    if (any(missing_year)) {
      gbif_filtered$year[missing_year] <- lubridate::year(gbif_filtered$date[missing_year])
    }
  } else {
    if (all(is.na(year_vals))) {
      if (all(is.na(event_dates))) {
        stop("No usable year/eventDate information to derive yearly bins.", call. = FALSE)
      }
      year_vals <- lubridate::year(event_dates)
    } else {
      missing_year <- is.na(year_vals) & !is.na(event_dates)
      if (any(missing_year)) {
        year_vals[missing_year] <- lubridate::year(event_dates[missing_year])
      }
    }
    gbif_filtered$year <- year_vals
    gbif_filtered$date <- as.Date(paste0(gbif_filtered$year, "-01-01"))
  }

  gbif_filtered <- gbif_filtered[!is.na(gbif_filtered$date) & !is.na(gbif_filtered$year), , drop = FALSE]
  if (!nrow(gbif_filtered)) {
    stop("No GBIF rows retained after constructing date/year columns.", call. = FALSE)
  }

  trs_path <- file.path(data_dir, sprintf("model_prep_%s_trs_daily.Rds", slug))
  if (!file.exists(trs_path)) {
    stop("TRS daily file not found at ", trs_path, call. = FALSE)
  }
  trs_daily <- readRDS(trs_path)
  if (!inherits(trs_daily, "sf")) {
    stop("TRS daily file must be an sf object.", call. = FALSE)
  }
  message("Loaded TRS daily geometry from ", trs_path)

  trs_pts <- trs_daily
  if (!"cell_id" %in% names(trs_pts)) {
    if ("TigacellID" %in% names(trs_pts)) {
      trs_pts$cell_id <- as.character(trs_pts$TigacellID)
    } else if ("trs_id" %in% names(trs_pts)) {
      trs_pts$cell_id <- as.character(trs_pts$trs_id)
    } else {
      coords_tmp <- sf::st_coordinates(trs_pts)
      trs_pts$cell_id <- paste0(round(coords_tmp[, 1], 6), "_", round(coords_tmp[, 2], 6))
    }
  }
  trs_pts <- trs_pts[!duplicated(trs_pts$cell_id), , drop = FALSE]

  gbif_sf <- sf::st_as_sf(
    gbif_filtered,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )
  trs_m <- sf::st_transform(trs_pts, 3857)
  gbif_m <- sf::st_transform(gbif_sf, 3857)

  nearest_idx <- sf::st_nearest_feature(gbif_m, trs_m)
  assigned_pts <- trs_m[nearest_idx, , drop = FALSE]

  if (!is.null(max_dist_m)) {
    dist_vals <- sf::st_distance(gbif_m, assigned_pts, by_element = TRUE)
    keep_idx <- as.numeric(dist_vals) <= max_dist_m
    gbif_m <- gbif_m[keep_idx, , drop = FALSE]
    assigned_pts <- assigned_pts[keep_idx, , drop = FALSE]
  }
  if (!nrow(gbif_m)) {
    stop("No GBIF records remained after nearest-point assignment/filtering.", call. = FALSE)
  }

  gbif_subset <- sf::st_drop_geometry(gbif_m)
  agg_df <- tibble::tibble(
    cell_id = assigned_pts$cell_id,
    date = as.Date(gbif_subset$date),
    year = as.integer(gbif_subset$year)
  )
  agg_df <- dplyr::count(agg_df, cell_id, date, year, name = weight_col)

  geom_lookup <- trs_m[, c("cell_id"), drop = FALSE]
  out_sf <- dplyr::left_join(agg_df, geom_lookup, by = "cell_id")
  out_sf <- sf::st_as_sf(out_sf)
  out_sf <- sf::st_transform(out_sf, 4326)

  out_sf <- out_sf[out_sf[[weight_col]] > 0, , drop = FALSE]
  if (!nrow(out_sf)) {
    stop("Target-group background aggregation produced zero rows.", call. = FALSE)
  }

  # Normalise core column types and expose centroid coordinates for reuse
  out_sf$date <- as.Date(out_sf$date)
  out_sf$year <- as.integer(out_sf$year)
  out_sf[[weight_col]] <- as.numeric(out_sf[[weight_col]])
  xy <- sf::st_coordinates(out_sf)
  out_sf$lon <- xy[, 1]
  out_sf$lat <- xy[, 2]

  output_path <- file.path(data_dir, sprintf("model_prep_%s_tgb_daily.Rds", slug))
  if (isTRUE(write_output)) {
    if (file.exists(output_path) && !isTRUE(overwrite)) {
      stop("Output already exists at ", output_path, " (set overwrite = TRUE to replace it).", call. = FALSE)
    }
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out_sf, output_path)
    message("Saved target-group background dataset to ", output_path)
  }

  attr(out_sf, "output_path") <- output_path
  attr(out_sf, "gbif_source") <- gbif_path
  attr(out_sf, "trs_source") <- trs_path

  out_sf
}