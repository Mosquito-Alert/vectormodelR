#' Build TRS daily sampling effort surface for a location
#'
#' Clips the Mosquito Alert sampling effort dataset to an administrative area
#' already prepared by `initialize_ma_dataset()`, writes the resulting TRS layer,
#' and saves auxiliary artefacts (minimum SE logit value and joined effort
#' metrics). The required Mosquito Alert vector presences are loaded from
#' `vector_<slug>_malert.Rds`, allowing the TRS artefact to be regenerated
#' independently of the full initialisation pipeline.
#'
#' @param iso3 Three-letter ISO3 code identifying the country.
#' @param admin_level Administrative level used when the grid was created.
#' @param admin_name Administrative unit name.
#' @param sampling_effort_url Remote CSV (gzipped) providing the Mosquito Alert
#'   sampling effort surface. Defaults to the canonical GitHub source.
#' @param vector_dir Directory containing `vector_<slug>_malert.Rds`. Defaults to
#'   "data/proc".
#' @param data_dir Directory where perimeter artefacts live and where the TRS
#'   outputs should be written. Defaults to "data/proc".
#' @param write_output Logical; when `TRUE` (default) write the TRS artefacts to
#'   disk.
#'
#' @return An `sf` object containing the clipped TRS daily surface, or `NULL`
#'   when the artefact cannot be produced.
#' @importFrom dplyr mutate filter left_join
#' @importFrom sf st_as_sf st_transform st_drop_geometry
#' @importFrom readr read_rds write_rds read_csv
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate year
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' artefacts <- build_trs_daily(
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona"
#' )
#' }
build_trs_daily <- function(
  iso3,
  admin_level,
  admin_name,
  sampling_effort_url = "https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz",
  vector_dir = "data/proc",
  data_dir = "data/proc",
  write_output = TRUE
) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Install it with install.packages('readr').", call. = FALSE)
  }
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

  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  slug <- ids$slug

  vector_path <- file.path(vector_dir, sprintf("vector_%s_malert.Rds", slug))
  if (!file.exists(vector_path)) {
    stop("Mosquito Alert vector dataset not found at ", vector_path, call. = FALSE)
  }
  message("• Loading Mosquito Alert vector data: ", vector_path)
  vector_raw <- readRDS(vector_path)
  vector_tbl <- tibble::as_tibble(vector_raw)

  required_cols <- c("lon", "lat", "creation_date")
  missing_cols <- setdiff(required_cols, names(vector_tbl))
  if (length(missing_cols)) {
    stop(
      "Mosquito Alert vector dataset is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  vector_tbl <- vector_tbl |>
    dplyr::mutate(
      lon = as.numeric(.data$lon),
      lat = as.numeric(.data$lat),
      date = lubridate::as_date(.data$creation_date)
    )

  needs_year <- !"year" %in% names(vector_tbl) || all(is.na(vector_tbl$year))
  if (needs_year) {
    vector_tbl <- vector_tbl |>
      dplyr::mutate(year = lubridate::year(.data$date))
  } else {
    vector_tbl <- vector_tbl |>
      dplyr::mutate(year = as.integer(.data$year))
  }

  prepared <- vector_tbl |>
    dplyr::filter(!is.na(.data$lon), !is.na(.data$lat), !is.na(.data$date)) |>
    dplyr::mutate(TigacellID_small = make_samplingcell_ids(.data$lon, .data$lat, 0.025)) |>
    dplyr::select("lon", "lat", "date", "year", "TigacellID_small")

  message("• Downloading sampling effort from ", sampling_effort_url)
  sampling_effort <- readr::read_csv(sampling_effort_url, show_col_types = FALSE)

  if (is.null(sampling_effort) || !nrow(sampling_effort)) {
    warning("Sampling effort dataset is empty.")
    return(NULL)
  }
  if (!all(c("masked_lon", "masked_lat") %in% names(sampling_effort))) {
    warning("Sampling effort dataset missing 'masked_lon'/'masked_lat' columns.")
    return(NULL)
  }

  message("• Clipping sampling effort to admin boundary")

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_stem <- file.path(data_dir, paste0("model_prep_", slug, "_"))

  sampling_effort_sf <- tryCatch(
    sf::st_as_sf(
      sampling_effort,
      coords = c("masked_lon", "masked_lat"),
      crs = 4326,
      remove = FALSE
    ),
    error = function(e) {
      warning("Failed to convert sampling effort to sf: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(sampling_effort_sf)) {
    warning("Failed to convert sampling effort to sf; skipping TRS build.")
    return(NULL)
  }

  candidate_maps <- file.path(
    data_dir,
    c(
      paste0("spatial_", slug, "_adm.Rds"),
      paste0("map_", slug, "_adm.Rds")
    )
  )
  map_path <- candidate_maps[file.exists(candidate_maps)][1]
  if (is.na(map_path)) {
    warning("No administrative map RDS found for slug '", slug, "'.")
    return(NULL)
  }
  message("• Using administrative map: ", map_path)

  admin_map <- readr::read_rds(map_path) |> sf::st_transform(4326)
  trs_daily <- sampling_effort_sf[admin_map, ]
  if (!nrow(trs_daily)) {
    warning("No sampling effort points intersect the administrative boundary for '", slug, "'.")
    return(NULL)
  }
  message("• Retained ", nrow(trs_daily), " sampling effort cells inside the boundary")

  if (isTRUE(write_output)) {
    trs_path <- paste0(output_stem, "trs_daily.Rds")
    message("• Writing TRS daily surface to ", trs_path)
    readr::write_rds(trs_daily, trs_path)
  }

  min_SE_logit <- NA_real_
  if ("SE" %in% names(trs_daily)) {
    positive_se <- trs_daily$SE[trs_daily$SE > 0 & trs_daily$SE < 1]
    if (length(positive_se) > 0) {
      min_SE <- min(positive_se, na.rm = TRUE)
      min_SE_logit <- log(min_SE / (1 - min_SE))
      if (isTRUE(write_output)) {
        se_path <- paste0(output_stem, "min_SE_logit.Rds")
        message("• Writing minimum SE logit to ", se_path)
        readr::write_rds(min_SE_logit, se_path)
      }
    }
  }

  d_se <- prepared |>
    dplyr::mutate(TigacellID = .data$TigacellID_small) |>
    dplyr::filter(.data$year >= 2018)

  joinable_effort <- sf::st_drop_geometry(trs_daily)
  join_cols <- intersect(c("TigacellID", "date"), names(joinable_effort))
  if (length(join_cols) > 0) {
    d_se <- d_se |>
      dplyr::left_join(joinable_effort, by = join_cols)
  }

  if (!isTRUE(write_output)) {
    message("• Skipped writing artefacts because write_output = FALSE")
  }

  trs_daily
}
